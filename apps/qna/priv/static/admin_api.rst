QNA HTTP API ドキュメント
==========================

概要
----

QNA HTTP API は、フロントエンドのJavaScriptアプリケーションからユーザーおよびIPアドレスを管理するためのエンドポイントを提供します。ErlangとCowboy Webサーバーを使用して構築されたこのAPIは、標準的なRESTful操作をサポートしています。本ドキュメントでは、利用可能なエンドポイント、その機能、リクエストパラメータ、および期待されるレスポンスについて説明します。

ベースURI
--------

フロントエンドのJavaScriptから同一オリジンへのリクエストを行う場合、完全なURLを指定する必要はありません。以下のような相対パスを使用します。

.. code-block:: javascript

    const BASE_URI = '/qna/api/v1';

エンドポイント
------------

### ユーザー管理

#### ユーザーの取得

- **エンドポイント:** `/qna/api/v1/users`
- **メソッド:** `GET`
- **説明:**  
  全登録ユーザーのリストを取得します。

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ:**
    .. code-block:: json

        {
          "success": true,
          "users": [
            "user1",
            "user2",
            // 他のユーザー...
          ]
        }

#### 新規ユーザーの追加

- **エンドポイント:** `/qna/api/v1/users`
- **メソッド:** `POST`
- **説明:**  
  システムに新しいユーザーを追加します。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **リクエストボディ:**
  
  .. code-block:: json

      {
        "id": "new_user_id",
        "is_admin": true,
      }

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ:**
    .. code-block:: json

        {
          "success": true,
          "passwd": "P@ssw0rd"
        }

#### ユーザーパスワードの更新

- **エンドポイント:** `/qna/api/v1/users/passwd`
- **メソッド:** `POST`
- **説明:**  
  既存ユーザーのパスワードを更新します。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **リクエストボディ:**
  
  .. code-block:: json

      {
        "id": "existing_user_id",
        "passwd": "new_password",
      }

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ:**
    .. code-block:: json

        {
          "success": true
        }

### IP管理

#### IPアドレスの取得

- **エンドポイント:** `/qna/api/v1/ip`
- **メソッド:** `GET`
- **説明:**  
  全登録IPアドレスのリストを取得します。

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ:**
    .. code-block:: json

        {
          "success": true,
          "ip_list": [
            "192.168.1.1",
            "192.168.1.2",
            // 他のIPアドレス...
          ]
        }

#### 新規IPアドレスの追加

- **エンドポイント:** `/qna/api/v1/ip`
- **メソッド:** `POST`
- **説明:**  
  システムに新しいIPアドレスを追加します。

- **リクエストヘッダー:**
  
  - `Content-Type: application/json`

- **リクエストボディ:**
  
  .. code-block:: json

      {
        "ip": "new_ip_address",
        "memo": "description"
      }

- **レスポンス:**
  
  - **ステータスコード:** `200 OK`
  - **ボディ:**
    .. code-block:: json

        {
          "success": true
        }

