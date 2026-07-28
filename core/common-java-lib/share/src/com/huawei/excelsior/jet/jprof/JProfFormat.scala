/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.jprof

import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType.BLAME_CALL_TREE
import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType.BLAME_CODE_UNIT_IDS
import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType.BLAME_HOTSPOT
import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType.BLAME_INLINE_CONTEXTS
import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType.BLAME_MARKED_REGIONS
import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType.BLAME_METHOD_HITS
import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType.BLAME_STATS
import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType.FIELDS_ENTRY
import com.huawei.excelsior.jet.jprof.JProfFormat.EntryType.USG_ENTRY
import com.huawei.excelsior.jet.jprof.JProfFormat.ObjType.BLAME_CALLER
import com.huawei.excelsior.jet.jprof.JProfFormat.ObjType.BLAME_CALL_NODE
import com.huawei.excelsior.jet.jprof.JProfFormat.ObjType.BLAME_CODE_UNIT_DEF
import com.huawei.excelsior.jet.jprof.JProfFormat.ObjType.BLAME_MARKER
import com.huawei.excelsior.jet.jprof.JProfFormat.ObjType.BLAME_METHOD
import com.huawei.excelsior.jet.jprof.JProfFormat.ObjType.BLAME_STATE
import com.huawei.excelsior.jet.jprof.JProfFormat.ObjType.BLAME_TARGET
import com.huawei.excelsior.jet.jprof.JProfFormat.ObjType.FIELDS_CLASS
import com.huawei.excelsior.jet.jprof.JProfFormat.ObjType.FIELDS_STATIC_FINAL_PRIM
import com.huawei.excelsior.jet.jprof.JProfFormat.SectionType.BLAME_PROF
import com.huawei.excelsior.jet.jprof.JProfFormat.SectionType.FIELDS_DATA
import com.huawei.excelsior.jet.jprof.JProfFormat.SectionType.USG_PROF

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/** JProf format descriptor.
  * <strict>WARNING:</strict> This format **MUST BE** always synchronized with its AJ copy in runtime:
  * see {@code com.huawei.excelsior.jet.runtime.features.profiler.jprof.JProfFormat}
  *
  * @author xappymah
  * @author afilatov
  * @author ijorch
  * @since 1.0
  */
object JProfFormat {

    private[jprof] val JPROF_INTERMEDIATE_EXT = ".inter.jprof"
    private[jprof] val JPROF_EXT = ".jprof"
    private[jprof] val JPROF_HITS_EXT = ".jprof.hits"
    private[jprof] val BACKUP_SUFFIX = ".bak"

    private[jprof] val HEADER = "@@@ JPROF @@@"
    private[jprof] val VERSION_PREFIX = "@@@ v"

    private[jprof] val SECTION_START = "@start"
    private[jprof] val SECTION_END = "@end"

    private[jprof] val ENTRY_START = "!"
    private[jprof] val ENTRY_END = "!end"

    /** The separator character between object type and attributes. */
    val OBJ_DEF_SEPARATOR = ' '

    /** The separator character between attributes. */
    val ATTRS_DEF_SEPARATOR = OBJ_DEF_SEPARATOR

    /** The separator character between key and value. */
    val KEY_VALUE_ASSIGN = '='

    private[jprof] val OBJ_INDENT = "  "

    private[jprof] val LINE_END = '\n'

    private[jprof] val COMMENT_LINE = "%"
    private[jprof] val COMMENT_BLOCK = "%%"

    /** Current version of JProf format. */
    val VERSION_CURRENT = "2.1.0" ensuring (_ == JProfVersion.current.pretty)

    /** A special value to designate unknown class. */
    val CLASS_UNKNOWN = "<unknown>"

    /** A special value to designate unknown method name. */
    val METHOD_NAME_UNKNOWN = "<unknown>"

    /** A special value to designate unknown method signature. */
    val METHOD_SIG_UNKNOWN = "(<unknown>)"

    // the only currently supported profiling type
    private[jprof] val PROF_TYPE_SOFTWARE = "prue"

    /** This enum defines jprof-section types. */
    enum SectionType(val sectionType: String) {

        case BLAME_PROF extends SectionType("Blame")
        
        case FIELDS_DATA extends SectionType("Fields")
        
        /**
          * !entry
          * name [method|class|refl|field|etc name]
          * mask [usg-mask in hex]
          * !end
          */
        case USG_PROF extends SectionType("Metaops")
    }

    object SectionType {
        def findSectionType(typeStr: String) = SectionType.values.find(_.sectionType == typeStr).orNull
    }

    /** This enum defines jprof-entries types and where they are allowed. */
    enum EntryType(val entryType: String, val rootSection: SectionType) {
        /* Blame profiler entries */
        case BLAME_STATS extends EntryType("stats", BLAME_PROF)
        case BLAME_CODE_UNIT_IDS extends EntryType("code_unit_ids", BLAME_PROF)
        case BLAME_METHOD_HITS extends EntryType("method_hits", BLAME_PROF)
        case BLAME_CALL_TREE extends EntryType("call_tree", BLAME_PROF)

        /* Fields data entries */
        case FIELDS_ENTRY extends EntryType("entry", FIELDS_DATA)

        /* USG profiler entries */
        case USG_ENTRY extends EntryType("entry", USG_PROF)

        /* Blame profiler entries that are deprecated but still in active use */
        case BLAME_HOTSPOT extends EntryType("hotspot", BLAME_PROF)
        case BLAME_MARKED_REGIONS extends EntryType("marked_regions", BLAME_PROF)
        case BLAME_INLINE_CONTEXTS extends EntryType("inline_contexts", BLAME_PROF)
    }

    object EntryType {
        def findEntryType(rootSection: SectionType, typeStr: String) = {
            def equalEntry(e: EntryType): Boolean = {
                e.entryType == typeStr && e.rootSection == rootSection
            }

            EntryType.values.find(equalEntry).orNull
        }
    }

    /** This enum defines jprof-object types and where they are allowed. */
    enum ObjType(val objType: String, val isDeprecated: Boolean, val formatDescription: String, val rootEntries: EntryType*) {
        /* Blame profiler objects */
        case BLAME_TYPE extends ObjType("prof_type", "prue|claire", BLAME_STATS)
        case BLAME_INSPECTIONS extends ObjType("inspections", "count", BLAME_STATS)
        case BLAME_SAMPLES extends ObjType("samples", "count", BLAME_STATS)
        case BLAME_HITS extends ObjType("hits", "count", BLAME_STATS)
        case BLAME_LOST extends ObjType("lost", "count", BLAME_STATS)
        case BLAME_INTERVAL extends ObjType("interval", "interval-microsec", BLAME_STATS)
        case BLAME_PRIORITY extends ObjType("profiler_priority", "num", BLAME_STATS)
        case BLAME_HEURISTICS extends ObjType("heuristics_allowed", "boolean", BLAME_STATS)
        case BLAME_MARK_FRAMES extends ObjType("mark_frames", "boolean", BLAME_STATS)
        case BLAME_YIELD_POLICY extends ObjType("yield_between_inspections", "boolean", BLAME_STATS)
        case BLAME_USED_MEM extends ObjType("used_mem", "bytes", BLAME_STATS)

        case BLAME_NATIVE extends ObjType("native", "count", BLAME_STATS)
        case BLAME_SAFE_SECTION extends ObjType("safe_section", "count", BLAME_STATS)
        case BLAME_SAFE_POINT extends ObjType("safe_point", "count", BLAME_STATS)
        case BLAME_FAILED_SUSPEND extends ObjType("failed_suspend", "count", BLAME_STATS)
        case BLAME_LOST_INTERPRETER extends ObjType("lost_interpreter", "count", BLAME_STATS)
        case BLAME_LOST_COMPILED extends ObjType("lost_compiled", "count", BLAME_STATS)
        case BLAME_LOST_UNKNOWN extends ObjType("lost_unknown", "count", BLAME_STATS)

        case BLAME_CODE_UNIT_DEF extends ObjType("code_unit_def", BLAME_CODE_UNIT_IDS)

        case BLAME_METHOD extends ObjType("method", BLAME_METHOD_HITS, BLAME_MARKED_REGIONS, BLAME_INLINE_CONTEXTS)
        case BLAME_STATE extends ObjType("state", BLAME_METHOD_HITS)
        case BLAME_CALL_NODE extends ObjType("call_node", BLAME_CALL_TREE)

        case BLAME_MARKER extends ObjType("marker", BLAME_MARKED_REGIONS)

        /* Fields data objects */
        case FIELDS_CLASS extends ObjType("class", FIELDS_ENTRY)
        case FIELDS_STATIC_FINAL_PRIM extends ObjType("sfp", FIELDS_ENTRY)

        /* USG profiler objects */
        case USG_NAME extends ObjType("name", USG_ENTRY)
        case USG_MASK extends ObjType("mask", USG_ENTRY)

        /* Object types that are deprecated but still in use */
        case BLAME_TARGET extends ObjType("target", true, "", BLAME_HOTSPOT)
        case BLAME_CALLER extends ObjType("caller", true, "", BLAME_HOTSPOT)

        def this(objType: String, formatDescription: String, rootEntries: EntryType*) = {
            this(objType, false, formatDescription, rootEntries:_*)
        }

        def this(objType: String, rootEntries: EntryType*) = {
            this(objType, false, "", rootEntries:_*)
        }

        val allowedRootEntries = Set.from(rootEntries)
    }

    object ObjType {
        def findObjType(rootEntry: EntryType, typeStr: String) = {
            def equalEntry(entry: ObjType) = {
                (entry.isDeprecated || entry.allowedRootEntries.contains(rootEntry)) && (entry.objType == typeStr)
            }

            ObjType.values.find(equalEntry).orNull
        }
    }

    /** Represents an object consisting of a pair of integers. */
    case class IntPair(x: Int, y: Int)

    object IntPair {
        private val INT_PAIR_VALUE_SEP = ":"

        private[jprof] def parse(data: String) = {
            val Array(x, y) = data.split(INT_PAIR_VALUE_SEP)
            IntPair(x.toInt, y.toInt)
        }
    }

    /** Keys of key-value pairs of an object */
    enum KeyName (val fieldName: String, val deserialize: (String) => Any, val fieldFormatDescription: String, val containingObjects: ObjType*) {

        case UNKNOWN extends KeyName("", "")

        /* Identifiers of report objects */
        case CUID extends KeyName("cuid", "string",
            BLAME_CODE_UNIT_DEF, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)

        case SCOPE_ID extends KeyName("scope-id", "string", BLAME_STATE)

        case NODE_ID extends KeyName("node-id", "string", BLAME_CALL_NODE)

        case CALLER_ID extends KeyName("caller-id", "string", BLAME_STATE, BLAME_CALL_NODE)
        
        /* Generic properties of classes */
        case RT_ANON extends KeyName("rt-anon", KeyName.parseBoolean, "boolean,false",
            FIELDS_CLASS,
            BLAME_CODE_UNIT_DEF, BLAME_TARGET, BLAME_CALLER, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)

        case CLASSLOADER_SID extends KeyName("clsid", "string,default",
            FIELDS_CLASS,
            BLAME_CODE_UNIT_DEF, BLAME_TARGET, BLAME_CALLER, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)

        case CLASS_NAME extends KeyName("class", "name",
            FIELDS_CLASS,
            BLAME_CODE_UNIT_DEF, BLAME_TARGET, BLAME_CALLER, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)

        case VERSIONED_FOR extends KeyName("versioned-for", "name",
            FIELDS_CLASS,
            BLAME_CODE_UNIT_DEF, BLAME_TARGET, BLAME_CALLER, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)


        /* Generic properties of code units, except properties of even more generic properties of their classes */
        case EXEC_KIND extends KeyName("kind", ExecutionKind.fromString,
            ExecutionKind.values.mkString("|"),
            BLAME_CODE_UNIT_DEF, BLAME_TARGET, BLAME_CALLER, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)

        case CALL_TYPE extends KeyName("call-type", MethodCallType.fromString,
            MethodCallType.values.mkString("|"),
            BLAME_CODE_UNIT_DEF, BLAME_TARGET, BLAME_CALLER, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)

        case AOT_AVAILABLE extends KeyName("aot-available", KeyName.parseBoolean, "boolean,false",
            BLAME_CODE_UNIT_DEF, BLAME_TARGET, BLAME_CALLER, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)

        case METHOD extends KeyName("method", "name+sig",
            BLAME_CODE_UNIT_DEF, BLAME_TARGET, BLAME_CALLER, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)


        /* Hit counts */
        case CALL_COUNT extends KeyName("call-count", KeyName.parseInt, "count,0", BLAME_CALL_NODE)

        case HEURISTIC_HITS extends KeyName("heuristic-hits", KeyName.parseInt, "count,0",
            BLAME_CALLER, BLAME_METHOD, BLAME_STATE)

        case INVALID_FRAME_HITS extends KeyName("invalid-frame-hits", KeyName.parseInt, "count,0",
            BLAME_METHOD, BLAME_STATE)

        case FOLLOWUP_COUNT extends KeyName("followup-count", KeyName.parseInt, "count,0", BLAME_CALL_NODE)

        case FOLLOWUP_HITS extends KeyName("followup-hits", KeyName.parseInt, "count,0",
            BLAME_TARGET, BLAME_CALLER, BLAME_METHOD, BLAME_STATE)

        case STALLED_HITS extends KeyName("stalled-hits", KeyName.parseInt, "count,0",
            BLAME_TARGET, BLAME_METHOD, BLAME_METHOD)

        case SIBERIAN_HITS extends KeyName("siberian-hits", KeyName.parseInt, "count,0",
            BLAME_TARGET, BLAME_METHOD, BLAME_METHOD)


        case BC_IN_CALLER extends KeyName("bc-in-caller", KeyName.parseInt, "int",
            BLAME_STATE, BLAME_CALL_NODE, BLAME_METHOD, BLAME_MARKER)

        case BC_IN_SCOPE extends KeyName("bc-in-scope", KeyName.parseInt, "int",
            BLAME_STATE, BLAME_CALL_NODE, BLAME_METHOD, BLAME_MARKER)


        /* Misc code-related */
        case METHOD_SIZE extends KeyName("size", KeyName.parseInt, "bytes", BLAME_TARGET, BLAME_METHOD)

        case SIBERIA_OFFSET extends KeyName("siberia-offset", KeyName.parseInt, "int,-1",
            BLAME_CODE_UNIT_DEF, BLAME_TARGET, BLAME_METHOD)

        case INLINED extends KeyName("inlined", KeyName.parseBoolean, "boolean,false",
            BLAME_METHOD, BLAME_CALL_NODE)

        case HEURISTIC extends KeyName("heuristic", KeyName.parseBoolean, "boolean,false",
            BLAME_CALL_NODE, BLAME_STATE)

        case INLINE_ROOT extends KeyName("root", KeyName.parseBoolean, "boolean,false",
            BLAME_TARGET, BLAME_METHOD)

        case FORCED_INLINE extends KeyName("forced", KeyName.parseBoolean, "boolean,false",
            BLAME_CALLER, BLAME_STATE)

        case EDGE_TYPE extends KeyName("type", "string", BLAME_CALLER, BLAME_STATE)

        case REGION_ID extends KeyName("id", KeyName.parseInt, "int",
            BLAME_STATE, BLAME_MARKER)


        /* Debugging attributes that may shed light where the report object comes from */
        case RAW_DETAILS extends KeyName("raw-details", "string",
            FIELDS_CLASS,
            BLAME_CODE_UNIT_DEF, BLAME_METHOD, BLAME_STATE, BLAME_CALL_NODE)

        case IS_BAD extends KeyName("is-bad", "boolean,false",
            BLAME_CODE_UNIT_DEF, BLAME_MARKER, BLAME_STATE, BLAME_CALL_NODE)

        case IS_WEIRD extends KeyName("is-weird", "boolean,false",
            BLAME_CODE_UNIT_DEF, BLAME_MARKER, BLAME_STATE, BLAME_CALL_NODE)


        /* Code region specific, deprecated but still in active use */
        case REGION_HITS extends KeyName("hits", KeyName.parseInt, "int,0", BLAME_MARKER)

        case REGION_DUPLICATE_POS extends KeyName("duplicate", KeyName.parseInt, "int,none", BLAME_MARKER)


        /* Static field specific */
        case FIELD_NAME extends KeyName("name", "string", FIELDS_STATIC_FINAL_PRIM)

        case FIELD_SIG extends KeyName("sig", "string", FIELDS_STATIC_FINAL_PRIM)

        case FIELD_VAL extends KeyName("val", KeyName.parseInt, "int", FIELDS_STATIC_FINAL_PRIM)


        /* Deprecated but still in active use */
        case INITIAL_HITS extends KeyName("initial-hits", KeyName.parseInt, "count,0",
            BLAME_METHOD, BLAME_STATE, BLAME_TARGET, BLAME_CALLER)

        case BC extends KeyName("bc", KeyName.parseInt, "int",
            BLAME_CALLER, BLAME_METHOD, BLAME_MARKER)

        case IC extends KeyName("ic", IntPair.parse, "start:end",
            BLAME_CALLER, BLAME_METHOD, BLAME_MARKER, BLAME_STATE, BLAME_CALL_NODE)


        def this(name: String, desc: String, objects: ObjType*)= {
            this(name, x => x, desc, objects:_*)
        }

        override def toString: String = fieldName

        private[jprof] def toDescription: String = s"$fieldName=[$fieldFormatDescription]"

        private[jprof] def serialize(x: Any): String = s"$fieldName$KEY_VALUE_ASSIGN$x"
    }

    object KeyName {
        def parseInt(str: String) = str.toInt

        def parseBoolean(str: String) = str.toBoolean

        /** Finds and returns [[KeyName]] by its serialized key (aka `fieldName`).*/
        def fromString(key: String): Option[KeyName] = values.find(_.fieldName == key)
    }

    /** Types of executable code */
    enum ExecutionKind(val value: String) {
        case AOT_COMPILED extends ExecutionKind("aot")
        case JIT_COMPILED extends ExecutionKind("jit")
        case INTERPRETED  extends ExecutionKind("int")
        case NATIVE       extends ExecutionKind("native")
        case UNKNOWN      extends ExecutionKind("<unknown>")
    }

    object ExecutionKind {
        def fromString(key: String): ExecutionKind = values.find(_.value == key).orNull
    }

    /** Calling conventions */
    enum MethodCallType(val value: String) {
        case JAVA extends MethodCallType("java")
        case CANGJIE extends MethodCallType("cangjie")
        case SCALA extends MethodCallType("scala")
        case AJ_MANAGED extends MethodCallType("aj_managed")
        case UNMANAGED extends MethodCallType("unmanaged")
        case UNKNOWN extends MethodCallType("unknown")
    }

    object MethodCallType {
        def fromString(key: String): MethodCallType = values.find(_.value == key).orNull
    }
}
