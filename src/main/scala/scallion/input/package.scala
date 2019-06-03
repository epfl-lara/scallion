/* Copyright 2019 EPFL, Lausanne
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package scallion

 /** This package contains utilities for handling input sources and positions.
   *
   * {{{
   * // Building a source from a string:
   * val stringSource = Source.fromString("[1, 2, 3]")
   *
   * // Or from a file:
   * val fileSource = Source.fromFile("data/clients.json")
   *
   * // With a custom positioner:
   * val customSource = Source.fromFile("data/clients.json", IndexPositioner)
   * }}}
   *
   * @groupprio source 1
   * @groupname source Sources
   *
   * @groupprio position 2
   * @groupname position Positions
   *
   * @groupprio positioner 3
   * @groupname positioner Positioners
   */
package object input