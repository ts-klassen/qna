QNA Master HTTP API ドキュメント
==================================

概要
----

QNA Master HTTP API は、マスタデータの取得とメンテナンスをするためのエンドポイントを提供します。このAPIは、ErlangとCowboy Webサーバーを使用して構築されており、標準的なRESTful操作をサポートしています。本ドキュメントでは、利用可能なエンドポイント、その機能、リクエストパラメータ、および期待されるレスポンスについて説明します。

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

### 製品マスタ

#### 製品マスタの取得

- **エンドポイント:** `/qna/api/v2/master/products`
- **メソッド:** `GET`
- **説明:**  
  製品マスタを取得します。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        {
          "success": true,
          "payload": {
              "products": [
                  {
                      "id": "1b51743f-9a19-4af8-90ba-3d5dac71ffe8",
                      "name": "product_id",
                      "versions": ["1.0.0", "1.0.1", "1.1.0"]
                  }
              ]
          },
          "rev": "1-c5b685b2af9c5f7a2026bbd579026bce" // 無い場合もあります。
        }

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "conflict" // 他のエラー理由として "server_error", "clause_error" があります。
            }

#### 製品マスタの更新

- **エンドポイント:** `/qna/api/v2/master/products`
- **メソッド:** `POST`
- **説明:**  
  製品マスタを上書きします。
  部分更新ではなく、 payload で全体更新します。**既存の製品も含めてPOSTしてください。**

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **リクエストボディ:**
  
  .. code-block:: json

        {
          "payload": {
              "products": [
                  {
                      "id": "1b51743f-9a19-4af8-90ba-3d5dac71ffe8",
                      "name": "product_id",
                      "versions": ["1.0.0", "1.0.1", "1.1.0"]
                  },
                  // ここに既存の製品も含めてください
              ]
          },
          "rev": "1-c5b685b2af9c5f7a2026bbd579026bce" // get したときの値。無ければ省略。
        }

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        {
          "success": true
        }

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "conflict" // 他のエラー理由として "server_error", "clause_error" があります。
            }



### 部署マスタ

#### 部署マスタの取得

- **エンドポイント:** `/qna/api/v2/master/departments`
- **メソッド:** `GET`
- **説明:**  
部署マスタを取得します。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        {
          "success": true,
          "payload": {
              "departments": [
                  {
                      "id": "1b51743f-9a19-4af8-90ba-3d5dac71ffe8",
                      "name": "human resources"
                  }
              ]
          },
          "rev": "1-c5b685b2af9c5f7a2026bbd579026bce" // 無い場合もあります。
        }

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "conflict" // 他のエラー理由として "server_error", "clause_error" があります。
            }

#### 部署マスタの更新

- **エンドポイント:** `/qna/api/v2/master/departments`
- **メソッド:** `POST`
- **説明:**  
部署マスタを上書きします。
  部分更新ではなく、 payload で全体更新します。**既存の製品も含めてPOSTしてください。**

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **リクエストボディ:**
  
  .. code-block:: json

        {
          "payload": {
              "departments": [
                  {
                      "id": "1b51743f-9a19-4af8-90ba-3d5dac71ffe8",
                      "name": "human resources"
                  },
                  // ここに既存の部署も含めてください
              ]
          },
          "rev": "1-c5b685b2af9c5f7a2026bbd579026bce" // get したときの値。無ければ省略。
        }

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        {
          "success": true
        }

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "conflict" // 他のエラー理由として "server_error", "clause_error" があります。
            }
### シート項目マスタ

#### シート項目マスタの取得

- **エンドポイント:** `/qna/api/v2/master/sheet_items`
- **メソッド:** `GET`
- **説明:**  
シート項目マスタを取得します。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        {
          "success": true,
          "payload": {
              "sheet_items": [
                  {
                      "id": "1b51743f-9a19-4af8-90ba-3d5dac71ffe8",
                      "name": "sheet name",
                      "is_hidden": false
                  }
              ]
          },
          "rev": "1-c5b685b2af9c5f7a2026bbd579026bce" // 無い場合もあります。
        }

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "conflict" // 他のエラー理由として "server_error", "clause_error" があります。
            }

#### シート項目マスタの更新

- **エンドポイント:** `/qna/api/v2/master/sheet_items`
- **メソッド:** `POST`
- **説明:**  
シート項目マスタを上書きします。
  部分更新ではなく、 payload で全体更新します。**既存の製品も含めてPOSTしてください。**

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **リクエストボディ:**
  
  .. code-block:: json

        {
          "payload": {
              "sheet_items": [
                  {
                      "id": "1b51743f-9a19-4af8-90ba-3d5dac71ffe8",
                      "name": "sheet name",
                      "is_hidden": false
                  },
                  // ここに既存のシート項目も含めてください
              ]
          },
          "rev": "1-c5b685b2af9c5f7a2026bbd579026bce" // get したときの値。無ければ省略。
        }

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ（成功時）:**
    .. code-block:: json

        {
          "success": true
        }

      - **ボディ（失敗時）:**
        .. code-block:: json

            {
              "success": false,
              "reason": "conflict" // 他のエラー理由として "server_error", "clause_error" があります。
            }

### 注意事項

フロントエンドでは、以下の点に注意してデータを扱ってください。

- **HTMLインジェクション対策:**
  - データをバックエンドに送信する際はそのまま送信しますが、受信後に表示する際には適切にエスケープ処理を行ってください。

- **バリデーション:**
  - リクエストデータに対して、必要なフィールドが揃っているか、データ型が正しいかを確認してください。

- **データの上書き**
  - 更新APIは、部分更新ではなく全体更新です。必ず payload 全文を POST してください。既存の製品も含めて送信してください。

- **コンフリクト対策**
  - 表示する際に取得した rev を、更新時に渡してください。表示してから更新するまでの間に変更があった場合は、 conflict エラーを出します。更新後は、 rev を再取得してください。

