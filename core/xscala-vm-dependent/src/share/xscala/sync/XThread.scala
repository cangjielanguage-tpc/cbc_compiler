/*
 * Copyright (c) Huawei Technologies Co., Ltd. 2026. All rights reserved.
 * This source file is part of the Cangjie project, licensed under Apache-2.0
 * with Runtime Library Exception.
 *
 * See https://cangjie-lang.cn/pages/LICENSE for license information.
 */

package xscala.sync

trait XThread extends Thread

object XThread {

  /**
    * Creates non-daemon thread.
    *
    * @param body body of the thread to be executed.
    */
  def apply(body: => Unit): XThread =
    new Thread(() => body) with XThread

  /**
    * Creates non-daemon thread.
    *
    * @param name of the thread (used for debugging)
    * @param body body of the thread to be executed.
    * @throws NullPointerException iff name is  <pre>null</pre>
    */
  def apply(name: String)(body: => Unit): XThread =
    new Thread(() => body, name) with XThread
}
