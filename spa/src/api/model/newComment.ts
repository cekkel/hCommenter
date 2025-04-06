/**
 * Generated by orval v7.8.0 🍺
 * Do not edit manually.
 * hCommenter.API
 * An API for creating and retrieving conversation comments.
 * OpenAPI spec version: 1.0
 */

export interface NewComment {
  message: string;
  /**
   * @minimum -9223372036854776000
   * @maximum 9223372036854776000
   */
  parent?: number;
  author: string;
  convoUrl: string;
}
