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
            document.getElementById('questionForm').reset();
            // フォームリセット後に初期のフィールドを1つ保持
            resetDynamicFields();
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

// フォームリセット後に動的フィールドを初期化
function resetDynamicFields() {
    const titlesContainer = document.getElementById('titlesContainer');
    const notesContainer = document.getElementById('notesContainer');

    // タイトルフィールドのリセット
    titlesContainer.innerHTML = '';
    const initialTitleDiv = document.createElement('div');
    initialTitleDiv.className = 'dynamic-field';

    const titleInput = document.createElement('input');
    titleInput.type = 'text';
    titleInput.name = 'titles';
    titleInput.required = true;

    const addTitleButton = document.createElement('button');
    addTitleButton.type = 'button';
    addTitleButton.className = 'add-button';
    addTitleButton.textContent = '+';
    addTitleButton.onclick = () => addField('titles');

    const removeTitleButton = document.createElement('button');
    removeTitleButton.type = 'button';
    removeTitleButton.className = 'remove-button';
    removeTitleButton.textContent = '−';
    removeTitleButton.onclick = function() { removeField(this); };

    initialTitleDiv.appendChild(titleInput);
    initialTitleDiv.appendChild(addTitleButton);
    initialTitleDiv.appendChild(removeTitleButton);
    titlesContainer.appendChild(initialTitleDiv);

    // 備考フィールドのリセット
    notesContainer.innerHTML = '';
    const initialNotesDiv = document.createElement('div');
    initialNotesDiv.className = 'dynamic-field';

    const notesInput = document.createElement('input');
    notesInput.type = 'text';
    notesInput.name = 'notes';

    const addNotesButton = document.createElement('button');
    addNotesButton.type = 'button';
    addNotesButton.className = 'add-button';
    addNotesButton.textContent = '+';
    addNotesButton.onclick = () => addField('notes');

    const removeNotesButton = document.createElement('button');
    removeNotesButton.type = 'button';
    removeNotesButton.className = 'remove-button';
    removeNotesButton.textContent = '−';
    removeNotesButton.onclick = function() { removeField(this); };

    initialNotesDiv.appendChild(notesInput);
    initialNotesDiv.appendChild(addNotesButton);
    initialNotesDiv.appendChild(removeNotesButton);
    notesContainer.appendChild(initialNotesDiv);
}
