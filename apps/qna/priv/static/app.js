// ページロード時にsheat_idを一度生成して固定
const sheat_id = generateUUID();

// フィールド管理クラス
class FieldManager {
    constructor(containerId, type) {
        this.container = document.getElementById(containerId);
        this.type = type;
    }

    addField() {
        const fieldDiv = document.createElement('div');
        fieldDiv.className = 'dynamic-field';

        const input = document.createElement('input');
        input.type = 'text';
        input.name = this.type;
        // titlesは必須ではなくなったため、requiredを削除
        // notesも任意
        input.required = false;

        const addButton = document.createElement('button');
        addButton.type = 'button';
        addButton.className = 'add-button';
        addButton.textContent = '+';
        addButton.onclick = () => this.addField();

        const removeButton = document.createElement('button');
        removeButton.type = 'button';
        removeButton.className = 'remove-button';
        removeButton.textContent = '−';
        removeButton.onclick = () => this.removeField(fieldDiv);

        fieldDiv.appendChild(input);
        fieldDiv.appendChild(addButton);
        fieldDiv.appendChild(removeButton);

        this.container.appendChild(fieldDiv);
    }

    removeField(fieldDiv) {
        if (this.container.childElementCount > 1) { // 最低1つは残す
            this.container.removeChild(fieldDiv);
        }
    }

    clearFields() {
        // 見出し titles はクリアしない
        if (this.type === 'notes') {
            const notesInputs = this.container.querySelectorAll(`input[name="${this.type}"]`);
            notesInputs.forEach(input => {
                input.value = '';
            });
        }
    }
}

const titlesManager = new FieldManager('titlesContainer', 'titles');
const notesManager = new FieldManager('notesContainer', 'notes');

document.getElementById('questionForm').addEventListener('submit', async function(event) {
    event.preventDefault();

    const submitButton = document.getElementById('submitButton');
    submitButton.disabled = true; // ボタンを無効化
    submitButton.textContent = '送信中...'; // ボタンテキストを変更

    const productNameInput = document.getElementById('productName');
    const productVersionInput = document.getElementById('productVersion');
    const noInput = document.getElementById('questionNo');
    const no = noInput.value.trim();
    const question = document.getElementById('question').value.trim();

    // すべてのタイトル入力フィールドを取得し、空文字列を除外
    const titles = Array.from(document.querySelectorAll('input[name="titles"]'))
                        .map(input => input.value.trim())
                        .filter(title => title !== '');

    // すべての備考入力フィールドを取得し、空文字列を除外
    const notes = Array.from(document.querySelectorAll('input[name="notes"]'))
                        .map(input => input.value.trim())
                        .filter(note => note !== '');

    const qnaData = {
        embe_metadata: {
            product_name: productNameInput.value.trim(),
            product_version: productVersionInput.value.trim(),
            sheat_id: sheat_id,
            no: no,
            titles: titles,
            question: question,
            notes: notes
        },
        waiting_for: {
            embed: true,
            search: true,
            ai_answer: true
        }
    };

    try {
        const response = await fetch('/qna/api/v2/qna/upsert', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            credentials: 'same-origin',
            body: JSON.stringify(qnaData)
        });

        const result = await response.json();
        const messageDiv = document.getElementById('responseMessage');

        if (result.success) {
            messageDiv.style.color = 'green';
            messageDiv.textContent = '質問が正常に登録されました。';
            // 識別番号、質問主文、備考のみをクリア
            noInput.value = '';
            document.getElementById('question').value = '';
            notesManager.clearFields();
            // 製品名とバージョンをdisabledにする
            if (!productNameInput.disabled && !productVersionInput.disabled) {
                disableFixedFields(productNameInput, productVersionInput);
            }
        } else {
            messageDiv.style.color = 'red';
            messageDiv.textContent = `エラー: ${result.reason}`;
        }
    } catch (error) {
        console.error('Error:', error);
        const messageDiv = document.getElementById('responseMessage');
        messageDiv.style.color = 'red';
        messageDiv.textContent = 'サーバーとの通信中にエラーが発生しました。';
    } finally {
        submitButton.disabled = false; // ボタンを再度有効化
        submitButton.textContent = '質問を登録'; // ボタンテキストを元に戻す
    }
});

// UUID生成関数
function generateUUID() {
    // 簡易的なUUID生成
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
        const r = Math.random() * 16 | 0, v = c === 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
}

// フィールドを追加する関数
function addField(type) {
    if (type === 'titles') {
        titlesManager.addField();
    } else if (type === 'notes') {
        notesManager.addField();
    }
}

// フィールドを削除する関数
function removeField(button) {
    const fieldDiv = button.parentElement;
    const type = fieldDiv.parentElement.id === 'titlesContainer' ? 'titles' : 'notes';
    if (type === 'titles') {
        titlesManager.removeField(fieldDiv);
    } else if (type === 'notes') {
        notesManager.removeField(fieldDiv);
    }
}

// 製品名と製品バージョンのフィールドをdisabledにする関数
function disableFixedFields(productNameInput, productVersionInput) {
    productNameInput.disabled = true;
    productVersionInput.disabled = true;
}
