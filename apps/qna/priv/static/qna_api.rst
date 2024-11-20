QNA User HTTP API ドキュメント
==============================

概要
----

QNA User HTTP API は、一般ユーザー向けにQNAデータを操作するためのエンドポイントを提供します。このAPIは、ErlangとCowboy Webサーバーを使用して構築されており、標準的なRESTful操作をサポートしています。本ドキュメントでは、利用可能なエンドポイント、その機能、リクエストパラメータ、および期待されるレスポンスについて説明します。

ベースURI
--------

フロントエンドのJavaScriptから同一オリジンへのリクエストを行う場合、完全なURLを指定する必要はありません。以下のような相対パスを使用します。

.. code-block:: javascript

    const BASE_URI = '/qna/api/v2';

### 認証について

このAPIエンドポイントを利用するためには、サイトにアクセスした時点でBasic認証に成功している必要があります。フロントエンドからのリクエストには、`credentials: 'same-origin'` を指定してください。

.. code-block:: javascript

    fetch('/qna/api/v2/endpoint', {
        method: 'POST',
        credentials: 'same-origin'
    })

エンドポイント
------------

### QNA管理

#### QNAの作成または更新（Upsert）

- **エンドポイント:** `/qna/api/v2/qna/upsert`
- **メソッド:** `POST`
- **説明:**  
  QNAエントリを作成または更新します。既存のQNAが存在する場合は更新され、存在しない場合は新規作成されます。
  サーバー側で自動入力されるフィールドは、送信しても無視されます。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **リクエストボディ:**
  
  .. code-block:: json

      {
        "_id": "unique_qna_id", // optional on insert
        "_rev": "unique_qna_revision", // optional on insert
        "C": "2023-10-01T12:00:00.000+09:00" // ignored by backend
        "U": "2023-10-01T12:00:00.000+09:00" // ignored by backend
        "embe_id": "unique_embe_id", // ignored by backend
        "embe_metadata": {
          "available": true,
          "deleted": false,
          "product_id": "ProductX",
          "product_version": "1.0.0", // string
          "sheet_id": "sheet_123",
          "no": "001",
          "ordered_id": 1001, // integer
          "input": "Generated text_to_vector input", // ignored by backend
          "titles": ["Title1", "Title2"],
          "question": "What is the purpose of API?",
          "notes": ["Note1", "Note2"],
          "qna_id": "unique_qna_id" // ignored by backend
        },
        "answer": "This is the answer.", // optional
        "answer_sup": ["Supplementary answer 1", "Supplementary answer 2"], // optional
        "state": "ai_answered", // "human_answered" or "human_checked"
        "last_log_statement": "Entry created",
        "last_exec": { // ignored by backend
          "type": "embed",
          "at": "2023-10-01T12:00:00.000+09:00"
        },
        "waiting_for": {
          "embed": false,
          "search": true,
          "ai_answer": false
        },
        "last_search_result": ["search_id_1", "search_id_2"], // ignored by backend
        "qna_version": 1, // ignored by backend
        "log": [ // ignored by backend
          {
            "type": "create",
            "time": "2023-10-01T12:00:00Z",
            "user": "user123",
            "payload": {}
          }
        ]
      }

      **フィールドの詳細:**
      - `_id`: エントリの一意識別子。新規作成時は省略可能。
      - `_rev`: レビジョン番号。競合検知のために使用されます。更新時に必要。
      - `embe_metadata.input`: デバッグ目的。フロントエンドでは基本的に無視してください。
      - `waiting_for`: バッチ処理の状態を示します。作成時はすべて `true` に設定し、処理完了後に `false` になります。
        - `embed`: 埋め込み処理を待機中かどうか
        - `search`: 類似の過去回答検索を待機中かどうか
        - `ai_answer`: AIによる解答生成を待機中かどうか

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        {
          "success": true,
          "qna": {
            "_id": "unique_qna_id",
            "_rev": "unique_qna_revision",
            "C": "2023-10-01T12:00:00.000+09:00" // created time
            "U": "2023-10-01T12:00:00.000+09:00" // updated time
            "embe_id": "unique_embe_id",
            "embe_metadata": {
              "available": true,
              "deleted": false,
              "product_id": "ProductX",
              "product_version": "1.0.0",
              "sheet_id": "sheet_123",
              "no": "001",
              "ordered_id": 1001,
              "input": "Generated text_to_vector input",
              "titles": ["Title1", "Title2"],
              "question": "What is the purpose of API?",
              "notes": ["Note1", "Note2"],
              "qna_id": "qna_456"
            },
            "answer": "This is the answer.", // optional
            "answer_sup": ["Supplementary answer 1", "Supplementary answer 2"], // optional
            "state": "ai_answered",
            "last_log_statement": "Entry created",
            "last_exec": {
              "type": "embed",
              "at": "2023-10-01T12:00:00.000+09:00"
            },
            "waiting_for": {
              "embed": false,
              "search": true,
              "ai_answer": false
            },
            "last_search_result": ["search_id_1", "search_id_2"],
            "qna_version": 1,
            "log": [
              {
                "type": "create",
                "time": "2023-10-01T12:00:00Z",
                "user": "user123",
                "payload": {}
              }
            ]
          }
        }

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "conflict" // 他のエラー理由として "server_error", "clause_error" があります。
            }

#### QNAの取得（Lookup）

- **エンドポイント:** `/qna/api/v2/qna/lookup`
- **メソッド:** `POST`
- **説明:**  
  指定されたQNA ID ( `_id` フィールド ) に基づいてQNAエントリを取得します。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **リクエストボディ:**
  
  .. code-block:: json

      {
        "id": "qna_456"
      }

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        {
          "success": true,
          "qna": {
            "_id": "unique_qna_id",
            "_rev": "unique_qna_revision",
            "C": "2023-10-01T12:00:00.000+09:00" // created time
            "U": "2023-10-01T12:00:00.000+09:00" // updated time
            "embe_id": "unique_embe_id",
            "embe_metadata": {
              "available": true,
              "deleted": false,
              "product_id": "ProductX",
              "product_version": "1.0.0",
              "sheet_id": "sheet_123",
              "no": "001",
              "ordered_id": 1001,
              "input": "Generated text_to_vector input",
              "titles": ["Title1", "Title2"],
              "question": "What is the purpose of API?",
              "notes": ["Note1", "Note2"],
              "qna_id": "qna_456"
            },
            "answer": "This is the answer.", // optional
            "answer_sup": ["Supplementary answer 1", "Supplementary answer 2"], // optional
            "state": "ai_answered",
            "last_log_statement": "Entry created",
            "last_exec": {
              "type": "embed",
              "at": "2023-10-01T12:00:00.000+09:00"
            },
            "waiting_for": {
              "embed": false,
              "search": true,
              "ai_answer": false
            },
            "last_search_result": ["search_id_1", "search_id_2"],
            "qna_version": 1,
            "log": [
              {
                "type": "create",
                "time": "2023-10-01T12:00:00Z",
                "user": "user123",
                "payload": {}
              }
            ]
          }
        }

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "not_found" // 他のエラー理由として "server_error", "clause_error" があります。
            }


#### QNAの state 取得（state）

- **エンドポイント:** `/qna/api/v2/qna/state`
- **メソッド:** `GET`
- **説明:**  
  QNA state の一覧を取得します。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        { "success": true, state_list: ["init", "..."]}

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "server_error" // 他のエラー理由として "server_error", "clause_error" があります。
            }

#### QNAの一覧取得取得（list）

- **エンドポイント:** `/qna/api/v2/qna/list`
- **メソッド:** `POST`
- **説明:**  
  指定されたQNA state ( `state` フィールド ) に該当するQNA一覧を取得します。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **リクエストボディ:**
  
  .. code-block:: json

      {
        "state": "init"
      }

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        {
          "success": true,
          "qna_list": [{
            "_id": "unique_qna_id",
            "_rev": "unique_qna_revision",
            "C": "2023-10-01T12:00:00.000+09:00" // created time
            "U": "2023-10-01T12:00:00.000+09:00" // updated time
            "embe_id": "unique_embe_id",
            "embe_metadata": {
              "available": true,
              "deleted": false,
              "product_id": "ProductX",
              "product_version": "1.0.0",
              "sheet_id": "sheet_123",
              "no": "001",
              "ordered_id": 1001,
              "input": "Generated text_to_vector input",
              "titles": ["Title1", "Title2"],
              "question": "What is the purpose of API?",
              "notes": ["Note1", "Note2"],
              "qna_id": "qna_456"
            },
            "answer": "This is the answer.", // optional
            "answer_sup": ["Supplementary answer 1", "Supplementary answer 2"], // optional
            "state": "init",
            "last_log_statement": "Entry created",
            "last_exec": {
              "type": "embed",
              "at": "2023-10-01T12:00:00.000+09:00"
            },
            "waiting_for": {
              "embed": false,
              "search": true,
              "ai_answer": false
            },
            "last_search_result": ["search_id_1", "search_id_2"],
            "qna_version": 1,
            "log": [
              {
                "type": "create",
                "time": "2023-10-01T12:00:00Z",
                "user": "user123",
                "payload": {}
              }
            ]
          }]
        }

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "not_found" // 他のエラー理由として "server_error", "clause_error" があります。
            }

### データバリデーションとセキュリティ

フロントエンドでは、以下の点に注意してデータを扱ってください。

- **HTMLインジェクション対策:**
  - データをバックエンドに送信する際はそのまま送信しますが、受信後に表示する際には適切にエスケープ処理を行ってください。

- **バリデーション:**
  - リクエストデータに対して、必要なフィールドが揃っているか、データ型が正しいかを確認してください。

### その他の注意点

- **`qna_version` フィールド:**
  - 現在はバックエンドのスキーマバージョン管理に使用されています。フロントエンドではこのフィールドを操作する必要はありません。渡された場合でも無視されます。

- **ログ情報の活用:**
  - レスポンス内の `log` フィールドに含まれる情報は、ユーザー向けのチャット形式で表示する際に用います。

