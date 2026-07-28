/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.matching

import xscala.internal.ForeignRef0

import scala.annotation.static

/** Operations to work with regular expression entities using PCRE2 (Perl-compatible Regular Expressions) library. */
private[xscala] object Pcre {
  import Natives.*

  /** Reference to finalizable holder of some resource, which should be alive
    * when resource is obtained from foreign function or passed to it.
    */
  opaque type Anchor[T <: AnyRef] >: T = Object

  /** Handle for compiled regular expression.
    *
    * It should be released when no longer in use with [[Pcre]].[[delete]].
    */
  opaque type PcrexHandle = Long

  /** Creates [[PcrexHandle]] holding regular expression compiled from `pattern`.
    *
    * @param anchor instance of [[Pattern]] holding this [[PcrexHandle]] to guarantee
    *               that it's not reclaimed by GC during execution of foreign function.
    */
  @static @native def compile(pattern: String)(implicit anchor: Anchor[Pattern]): PcrexHandle

  /** Releases some [[PcrexHandle]].
    *
    * '''Notice''':
    *
    * after performing this method, [[PcrexHandle]] passed to it becomes invalid,
    * therefore, each unique handle must be released exactly once.
    */
  @static @native def delete(re: PcrexHandle): Unit

  extension (handle: PcrexHandle) {
    /** Searches next entry of this regular expression in `scope`.
      *
      * @param from start position of matching
      * @param anchor instance of [[Matcher]] holding this [[MatchData]] to guarantee
      *               that it's not reclaimed by GC during execution of foreign function
      */
    inline def search(scope: SearchScope)(from: Int, fullMatch: Boolean)(implicit anchor: Anchor[Matcher]): MatchInfo = {
      Pcre_search(handle, scope, from, fullMatch)(anchor)
    }
  }

  /** Matching string data (properly encoded representation, boundaries)
    * and cursor to query match region and capture groups.
    *
    * It should be released when no longer in use with [[SearchScope.free]].
    */
  opaque type SearchScope = ForeignRef0

  object SearchScope {
    /** Creates [[SearchScope]] for given matching string `s`.
      *
      * @param anchor instance of [[Matcher]] holding this [[SearchScope]] to guarantee
      *               that it's not reclaimed by GC during execution of foreign function.
      */
    inline def apply(handle: PcrexHandle, s: String)(implicit anchor: Anchor[Matcher]): SearchScope = SearchScope_apply(handle, s)(anchor)

    extension (scope: SearchScope) {
      /** Frees resources acquired by this [[SearchScope]].
        *
        * '''Notice''':
        *
        * after performing this method, receiver [[SearchScope]] becomes invalid,
        * therefore, each unique search scope must be freed exactly once.
        */
      inline def free(): Unit = SearchScope_free(scope)

      /** Returns count of groups captured by pattern. */
      inline def groupCount: Int = SearchScope_groupCount(scope)

      /** Returns index of first character of `group` captured by pattern, if last call of
        * [[PcrexHandle]].[[search]] for this scope succeed.
        *
        * This is unspecified by developers of PCRE2 library, what should be result of underlying foreign function,
        * if last call [[PcrexHandle]].[[search]] failed. Therefore, we need to report such execution state
        * as illegal.
        */
      inline def start(group: Int): Int = SearchScope_start(scope, group)

      /** Returns index of last character of `group` captured by pattern, if last call of
        * [[PcrexHandle]].[[search]] for this scope succeed.
        *
        * This is unspecified by developers of PCRE2 library, what should be result of underlying foreign function,
        * if last call [[PcrexHandle]].[[search]] failed. Therefore, we need to report such execution state
        * as illegal.
        */
      inline def end(group: Int): Int = SearchScope_end(scope, group)
    }
  }

  /** Result of [[PcrexHandle]].[[search]] operation. */
  opaque type MatchInfo = ForeignRef0

  extension (matchInfo: MatchInfo) {
    /** Search success indicator, `true` if search succeed, `false` otherwise. */
    inline def matches: Boolean = MatchInfo_getFirst(matchInfo) >= 0

    /** Index of first character of matched pattern, or `-1` if search failed. */
    inline def first: Int = MatchInfo_getFirst(matchInfo)

    /** Index of last character of matched pattern, or `-1` if search failed. */
    inline def last: Int = MatchInfo_getLast(matchInfo)

    /** Next position of search in modified UTF-8 bytes, or `-1` if search failed. */
    inline def nextOffset: Int = MatchInfo_getNextOffset(matchInfo)
  }

  private object Natives {
    @static @native def SearchScope_apply(re: PcrexHandle, str: String)(anchor: Anchor[Matcher]): SearchScope
    @static @native def SearchScope_free(scope: SearchScope): Unit

    @static @native def SearchScope_groupCount(scope: SearchScope): Int
    @static @native def SearchScope_start(scope: SearchScope, group: Int): Int
    @static @native def SearchScope_end(scope: SearchScope, group: Int): Int

    @static @native def Pcre_search(handle: PcrexHandle, scope: SearchScope, from: Int, fullMatch: Boolean)(anchor: Anchor[Matcher]): MatchInfo

    @static @native def MatchInfo_getFirst(info: MatchInfo): Int
    @static @native def MatchInfo_getLast(info: MatchInfo): Int
    @static @native def MatchInfo_getNextOffset(info: MatchInfo): Int
  }
  private class Natives
}

private class Pcre
