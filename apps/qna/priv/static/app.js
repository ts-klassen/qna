document.getElementById('questionForm').addEventListener('submit', async function(event) {
    event.preventDefault();

    const productName = document.getElementById('productName').value.trim();
    const productVersion = document.getElementById('productVersion').value.trim();
    const no = document.getElementById('questionNo').value.trim();
    const titlesInput = document.getElementById('titles').value.trim();
    const question = document.getElementById('question').value.trim();
    const notesInput = document.getElementById('notes').value.trim();

    const titles = titlesInput ? titlesInput.split(',').map(title => title.trim()) : [];
    const notes = notesInput ? notesInput.split(',').map(note => note.trim()) : [];

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
