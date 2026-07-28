/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package com.huawei.excelsior.jet.compiler.o2lib.u.PDB

import com.huawei.excelsior.jet.common.*
import com.huawei.excelsior.jet.compiler.o2lib.u.ErrMsg.*
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.xPDBModule.PDBKind
import com.huawei.excelsior.jet.compiler.o2lib.u.PDB.{xFSPlaceholdersModule as xFSPlaceholders, xPDBModule as xPDB}
import com.huawei.excelsior.jet.compiler.o2lib.u.{JStringsModule as js, xiEnvModule as env, xiFilesModule as xfs}
import com.huawei.excelsior.jet.compiler.o2lib.xmlib.FSModule as FS
import com.huawei.excelsior.jet.compiler.options.Option.SmartKind.*
import com.huawei.excelsior.o2s.runtime.O2SSupport.Keywords.*

import scala.collection.mutable.ArrayBuffer

/**
  Implements global runtime cache.
  
  Global runtime cache resides in user profile (or some other writable place)
  and may contain results of compilation of runtime by different compiler options.
  Cache is accumulative, so once a new runtime class is compiled 
  for the next compilation the cache is updated with the new content.
  
  The cache has an index file that contains version information and list 
  of compiler options sets: each item of the list enumerates options 
  that were used to get the particular runtime compilation. 
  The result of a particular compilation is saved in a zip file indexed 
  by order of appearance of the compilation in the index file.
*/
object RTCacheModule {
  private var INDEX_EXT: XString = js.newJString("index")

  private def getCachePlaceName(ext: XString): XString = {
    val cacheDir = env.getProfileWritablePath
    val cacheName = env.getRTCacheFileName
    FS.makeFileName(cacheDir, cacheName, ext)
  }

  private def getCachePlaceholder(ext: XString): xPDB.Placeholder =
    xFSPlaceholders.newFilenamePlaceholder(xPDB.manager.profilePDB, getCachePlaceName(ext))

  private def installInfo(): XString = {
    val verInfo = env.config.equation("version_info")
    // for release installations we validate rt cache by compatibility_info, 
    // while for work (night) builds we validate rt cache by version_info as  
    // it changes for every jet build (to not face with weird cache bugs during 
    // development).      
    if (verInfo.startsWith(js.newJString("jet-"), 0)) {
      js.newJString("release")
    } else {
      verInfo
    }
  }

  private def getRTCacheVersionString: XString = {
    val sb = new js.StringBuffer()
    sb.appendf("%S; Installation: %S;", env.config.equation("compatibility_info"), installInfo())
    sb.toJString
  }

     /**
    Calculates a string that contains all significant options of current compilation.
    Used as a key to search in the index file. 
  */
  private def calcCurCacheKey(): XString = {
    val sb = new js.StringBuffer()
    for (o <- env.config.options if o.checked == RuntimeRecompile || o.checked == AffectsCode) {
      sb.appendString(o.name)
      o match {
        case o: env.Option =>
          if (o.getBooleanValue) {
            sb.appendChar('+')
          } else {
            sb.appendChar('-')
          }

        case o: env.Equation =>
          sb.appendChar('=')
          sb.appendString(o.getStringValue)
      }
      sb.appendChar(';')
    }

    sb.toJString
  }

  /**
    Writes index file.
    index -- list of cache keys to be stored in the file. 
  */
  private def writeRTCacheIndex(index: ArrayBuffer[XString]): Unit = {
    val cacheIndexPlace = getCachePlaceholder(INDEX_EXT)
    val cacheIndexFile = cacheIndexPlace.openAsTextForWrite()
    cacheIndexFile.print("%S", getRTCacheVersionString)

    for (x <- index) {
      cacheIndexFile.print("\\n%S", x)
    }

    cacheIndexFile.closeNew()
  }

  /**
    Returns indexed extension of a zip file with object files. 
  */
  private def getCacheZipExt(number: Int): XString = js.format("%d.zip", number)

     /**
    Reads the index file.
    Returns the list of cache keys.
  */
  private def readRTCacheIndex(ignoreVersion: Boolean): ArrayBuffer[XString] = {
    val cacheIndexPlace = getCachePlaceholder(INDEX_EXT)
    if (!cacheIndexPlace.exists) {
      null
    } else {
      val cacheIndexFile = cacheIndexPlace.openAsTextForRead()
      val versionString = cacheIndexFile.readLine()
      if (versionString == null || !ignoreVersion && !versionString.equals(getRTCacheVersionString)) {
        cacheIndexFile.close()
        null
      } else {
        val index = new ArrayBuffer[XString]
        loop {
          val line = cacheIndexFile.readLine()
          if (line == null) {
            break()
          }
          index += line
        }
        cacheIndexFile.close()
        index
      }
    }
  }

  def cleanGlobalCache(): Unit = {
    val cacheIndex = getCachePlaceName(INDEX_EXT)

    if (xfs.sys.exists(cacheIndex)) {
      val index = readRTCacheIndex(ignoreVersion = true)
      if (index != null) {
        for (i <- index.indices) {
          val cacheZip = getCachePlaceName(getCacheZipExt(i))
          if (!xfs.sys.remove(cacheZip)) {
            env.errors.fault(ErrMsg599, cacheZip)
          }
        }
      }

      if (!xfs.sys.remove(cacheIndex)) {
        env.errors.fault(ErrMsg599, cacheIndex)
      }
    }
  }

  /**
    Looks up for an existing compilation cache in the cache index for current set of
    options. If finds then copies the compilation cache to local PDB and returns TRUE.  
  */
  def findGlobalRTCache(): Boolean = {
    val localCache = xPDB.manager.mainPDB.getContentHolder(xPDB.ContentType.CACHEDOBJ)
    if (localCache.exists) {
      // always clean local cache before using the new one 
      if (!localCache.delete()) {
        env.errors.fault(ErrMsg599, localCache.fullName)
      }
    }
    val index = readRTCacheIndex(ignoreVersion = false)
    if (index == null) {
      false
    } else {
      val curCacheKey = calcCurCacheKey()
      val curCacheNumber = index.indexOf(curCacheKey)
      if (curCacheNumber >= 0) {
        val curCacheExt = getCacheZipExt(curCacheNumber)
        val globalCache = getCachePlaceholder(curCacheExt)
        xPDB.copy(globalCache, localCache)
        true
      } else {
        false
      }
    }
  }

  /*
    Updates the cache for current compilation session.
    Copies the local PDB cache to the global and updates the index if needed.
  */
  def updateCache(): Unit = {
    var curCacheExt: XString = null

    val localCache = xPDB.manager.mainPDB.getContentHolder(xPDB.ContentType.CACHEDOBJ)
    assert(localCache.exists)

    var index = readRTCacheIndex(ignoreVersion = false)
    if (index == null) {
      cleanGlobalCache() // old cache does not exist or is not valid.  
                        // Clean it just in case before writing the new one.  
      index = new ArrayBuffer[XString]
    }

    val curCacheKey = calcCurCacheKey()
    val curCacheNumber = index.indexOf(curCacheKey)
    if (curCacheNumber >= 0) {
      curCacheExt = getCacheZipExt(curCacheNumber)
    } else {
      curCacheExt = getCacheZipExt(index.size)
      index += curCacheKey
      writeRTCacheIndex(index)
    }
    val globalCache = getCachePlaceholder(curCacheExt)
    xPDB.copy(localCache, globalCache)
  }
}
