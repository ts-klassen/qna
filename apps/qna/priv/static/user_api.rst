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
          "product_name": "ProductX",
          "product_version": "1.0.0", // string
          "sheat_id": "sheat_123",
          "no": "001",
          "ordered_id": 1001, // integer
          "input": "Generated text_to_vector input", // ignored by backend
          "titles": ["Title1", "Title2"],
          "question": "What is the purpose of API?",
          "notes": ["Note1", "Note2"],
          "qna_id": "unique_qna_id" // ignored by backend
        },
        "answer": "This is the answer.",
        "answer_sup": ["Supplementary answer 1", "Supplementary answer 2"],
        "state": "answered", // ignored by backend
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
              "product_name": "ProductX",
              "product_version": "1.0.0",
              "sheat_id": "sheat_123",
              "no": "001",
              "ordered_id": 1001,
              "input": "Generated text_to_vector input",
              "titles": ["Title1", "Title2"],
              "question": "What is the purpose of API?",
              "notes": ["Note1", "Note2"],
              "qna_id": "qna_456"
            },
            "answer": "This is the answer.",
            "answer_sup": ["Supplementary answer 1", "Supplementary answer 2"],
            "state": "answered",
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
          "reason": "conflict"
        }

#### QNAの取得（Lookup）

- **エンドポイント:** `/qna/api/v2/qna/lookup`
- **メソッド:** `POST`
- **説明:**  
  指定されたQNA IDに基づいてQNAエントリを取得します。

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
              "product_name": "ProductX",
              "product_version": "1.0.0",
              "sheat_id": "sheat_123",
              "no": "001",
              "ordered_id": 1001,
              "input": "Generated text_to_vector input",
              "titles": ["Title1", "Title2"],
              "question": "What is the purpose of API?",
              "notes": ["Note1", "Note2"],
              "qna_id": "qna_456"
            },
            "answer": "This is the answer.",
            "answer_sup": ["Supplementary answer 1", "Supplementary answer 2"],
            "state": "answered",
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
          "reason": "not_found"
        }

