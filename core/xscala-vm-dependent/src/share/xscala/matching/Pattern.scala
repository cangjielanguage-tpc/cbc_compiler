/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.matching

import xscala.vm.VMDependent

import scala.collection.mutable.ArrayBuffer
import scala.util.boundary
import scala.util.boundary.break

/** String pattern that can be matched against some character sequence or can be searched in some text.
  *
  * It's implied that such patterns are represented as compiled regular expressions.
  *
  * An implementation of regular expression patterns is required to provide at least the following constructs:
  *  - `.`, any character;
  *  - `X*`, ''zero'' or more repetitions of pattern `X`;
  *  - `X+`, ''one'' or more repetitions of pattern `X`;
  *  - `X?`, zero or one repetitions of pattern `X`;
  *  - escaping constructs with backslash character (`\`);
  *  - classes of matching characters:
  *    - `[abc]`, characters `a`, `b` or `c`;
  *    - `[^abc]`, any character that isn't `a`, `b` or `c`;
  *  - `[a-zA-Z]`, characters in inclusive ranges from `a` to `z` or from `A` to `Z`;
  *  - `^X`, sequence of characters ''starting'' with pattern `X`;
  *  - `X$`, sequence of characters ''ending'' with pattern `X`;
  *  - `\d`, characters representing digits from `0` to `9`;
  *  - `\t`, tab character;
  *  - `\n`, newline character;
  *  - `xhh`, character with hexadecimal value `0xhh`;
  *  - `\f`, the form-feed character;
  *  - `\r`, carriage-return character;
  *  - `\s`, whitespace characters (`[ \t\n\x0B\f\r]`);
  *  - `\S`, non-whitespace characters (`[^\s]`);
  *  - `X | Y`, either `X` or `Y`;
  *  - capturing and non-capturing groups (`(X)`, `(?:X)`).
  */
trait Pattern {
  /** Creates [[Matcher]] that will match string `s` against current pattern. */
  def matcher(s: String): Matcher

  /** Split given string `s` using `this` pattern instance as separator.
    * If the pattern matches the beginning of the string, the result array will begin with empty string,
    * unless the matched substring length is zero.
    *
    * Optional `limit` parameter allows fine-tuning the output:
    *  - Positive `limit` value is an upper bound on the number of elements in the result array.
    *  - Zero or negative `limit` value means there is no hard limit on the result element count.
    *  - Zero `limit` value will also remove trailing empty elements from the result array if at least one split was done.
    *
    * Note: if no split was done, the result will be an array with a single element `s`.
    */
  final def split(s: String, limit: Int = 0): Array[String] = {
    val limited = limit > 0
    val result = ArrayBuffer.empty[String]
    val m = matcher(s)
    var lastMatchEnd = 0

    boundary {
      while (m.find) {
        if (lastMatchEnd == 0 && m.end() == 0) {
          // Skip leading zero-width match
          assert(m.start() == 0)
        } else {
          result.addOne(s.substring(lastMatchEnd, m.start()))
          lastMatchEnd = m.end()
          if (limited && result.size == limit - 1) {
            // At this point we can have only one more element: the rest of the string.
            break()
          }
        }
      }
    }

    if (lastMatchEnd == 0) {
      // No match found, return the array with original string as a single element
      return Array[String](s)
    }

    // Finally, add remaining substring if possible
    if (!limited || result.size < limit) {
      result.addOne(s.substring(lastMatchEnd))
    }

    if (limit == 0) {
      // Trim trailing empty array elements away
      for (i <- (result.length - 1) to 0 by -1) {
        if (result(i) != "") {
          val length = i + 1
          val trimmed = Array.ofDim[String](length)
          result.copyToArray(trimmed, 0, length)
          return trimmed
        }
      }
      Array.empty[String]
    } else {
      result.toArray
    }
  }
}

object Pattern {
  /** Compiles regular expression `p` into a pattern. */
  @throws[PatternSyntaxException]
  def compile(p: String): Pattern = RegexCompiler.get.compile(p)

  /** Produces a literal pattern string for the specified [[p]].
    *
    * Resulting string can be used to create a pattern
    * matching [[p]] as literal sequence.
    */
  def quote(p: String): String = {
    val quotedChars = new ArrayBuffer[Char](p.length)

    for (c <- p) {
      c match {
        case '.' | '*' | '+' | '?' | '\\' | '[' | ']' | '^' | '$' | '|' | '{' | '}' | '(' | ')' =>
          quotedChars += '\\'

        case _ =>
      }

      quotedChars += c
    }

    new String(quotedChars.toArray)
  }
}

/** Occurs when a syntax error was found in the specified regular expression. */
case class PatternSyntaxException(pattern: String, description: String, index: Int) extends Exception {
  override def getMessage = if (index >= 0) s"$description near index $index\n$pattern" else s"$description\n$pattern"
}

/** Context for [[Pattern]] matching operations, such as:
  *  - matching pattern against some input;
  *  - searching pattern entries in text;
  *  - capture group data extraction.
  */
trait Matcher {
  /** Searches the next entry of current pattern in the current input.
    * Returns `true` if search succeed, `false` otherwise.
    */
  def find: Boolean

  /** Matches pattern against input.
    * Returns `true` if pattern fully matches the input, `false` otherwise.
    */
  def matches: Boolean

  /** Returns count of groups to be captured by current pattern.
    */
  @throws[Matcher.IllegalStateException]
  def groupCount: Int

  /** If previous search or matching operation was successful,
    * returns string, corresponding to capture group `g`,
    * throws [[Matcher.IllegalStateException]] otherwise.
    *
    * @param g index of requested capture group.
    *          Groups are indexed from `1`, `group(0)` retrieves the entire pattern entry.
    */
  @throws[Matcher.IllegalStateException]
  def group(g: Int = 0): String

  /** If previous search or matching operation was successful,
    * returns index of first character in capture group `g`,
    * throws [[Matcher.IllegalStateException]] otherwise.
    *
    * @param g index of requested capture group.
    *          Groups are indexed from `1`, `start(0)` retrieves
    *          index of first character of the entire pattern entry.
    */
  @throws[Matcher.IllegalStateException]
  def start(g: Int = 0): Int

  /** If previous search or matching operation was successful,
    * returns index of last character in capture group `g`,
    * throws [[Matcher.IllegalStateException]] otherwise.
    *
    * @param g index of requested capture group.
    *          Groups are indexed from `1`, `end(0)` retrieves
    *          index of last character of the entire pattern entry.
    */
  @throws[Matcher.IllegalStateException]
  def end(g: Int = 0): Int
}

object Matcher {
  /** Occurs when a requested operation cannot be performed because of the illegal state of the [[Matcher]]. */
  case class IllegalStateException(msg: String) extends Exception
}

private[xscala] trait RegexCompiler {
  /** Compiles regular expression `p` into a pattern. */
  def compile(p: String): Pattern
}

private[xscala] object RegexCompiler extends VMDependent[RegexCompiler]
