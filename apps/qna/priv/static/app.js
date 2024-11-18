document.getElementById('questionForm').addEventListener('submit', async function(event) {
    event.preventDefault();

    const productName = document.getElementById('productName').value.trim();
    const productVersion = document.getElementById('productVersion').value.trim();
    const no = document.getElementById('questionNo').value.trim();
    const question = document.getElementById('question').value.trim();

    // すべてのタイトル入力フィールドを取得
    const titles = Array.from(document.querySelectorAll('input[name="titles"]'))
                        .map(input => input.value.trim())
                        .filter(title => title !== '');

    // すべての備考入力フィールドを取得
    const notes = Array.from(document.querySelectorAll('input[name="notes"]'))
                        .map(input => input.value.trim())
                        .filter(note => note !== '');

    // 質問表IDを生成（例としてUUIDを使用）
    const sheat_id = generateUUID();

    const qnaData = {
        embe_metadata: {
            product_name: productName,
            product_version: productVersion,
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
            // 質問主文と備考のみをクリア
            document.getElementById('question').value = '';
            clearNotesFields();
        } else {
            messageDiv.style.color = 'red';
            messageDiv.textContent = `エラー: ${result.reason}`;
        }
    } catch (error) {
        console.error('Error:', error);
        const messageDiv = document.getElementById('responseMessage');
        messageDiv.style.color = 'red';
        messageDiv.textContent = 'サーバーとの通信中にエラーが発生しました。';
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
    const containerId = type === 'titles' ? 'titlesContainer' : 'notesContainer';
    const container = document.getElementById(containerId);

    const fieldDiv = document.createElement('div');
    fieldDiv.className = 'dynamic-field';

    const input = document.createElement('input');
    input.type = 'text';
    input.name = type;
    input.required = type === 'titles'; // titlesは必須、notesは任意

    const addButton = document.createElement('button');
    addButton.type = 'button';
    addButton.className = 'add-button';
    addButton.textContent = '+';
    addButton.onclick = () => addField(type);

    const removeButton = document.createElement('button');
    removeButton.type = 'button';
    removeButton.className = 'remove-button';
    removeButton.textContent = '−';
    removeButton.onclick = function() { removeField(this); };

    fieldDiv.appendChild(input);
    fieldDiv.appendChild(addButton);
    fieldDiv.appendChild(removeButton);

    container.appendChild(fieldDiv);
}

// フィールドを削除する関数
function removeField(button) {
    const fieldDiv = button.parentElement;
    const container = fieldDiv.parentElement;
    if (container.childElementCount > 1) { // 最低1つは残す
        container.removeChild(fieldDiv);
    }
}

// 備考フィールドをクリアする関数
function clearNotesFields() {
    const notesInputs = document.querySelectorAll('input[name="notes"]');
    notesInputs.forEach(input => {
        input.value = '';
    });
}
