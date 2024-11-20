// ページロード時にsheet_idを一度生成して固定
let sheet_id = generateUUID();

// 使用済みの識別番号を保持する配列
const usedNos = [];

// 次に採番する識別番号
let currentQuestionNo = 1;

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
        // titlesおよびnotesは必須ではなくなったため、requiredを削除
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
        // 見出し titles はクリアせず、備考 notes のみクリア
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

// 製品データの取得とセレクトボックスの初期化
async function initializeProductData() {
    try {
        const response = await fetch('/qna/api/v2/master/products', {
            method: 'GET',
            headers: {
                'Content-Type': 'application/json'
            },
            credentials: 'same-origin'
        });

        const result = await response.json();

        if (result.success && result.payload && result.payload.products) {
            populateProductSelect(result.payload.products);
        } else {
            console.error('製品データの取得に失敗しました。');
            showErrorMessage('製品データの取得に失敗しました。ページをリロードして再試行してください。');
        }
    } catch (error) {
        console.error('Error fetching products:', error);
        showErrorMessage('製品データの取得中にエラーが発生しました。');
    }
}

function populateProductSelect(products) {
    const productSelect = document.getElementById('productId');
    products.forEach(product => {
        const option = document.createElement('option');
        option.value = product.id;
        option.textContent = product.name;
        productSelect.appendChild(option);
    });

    // イベントリスナーを追加してバージョンを更新
    productSelect.addEventListener('change', function() {
        const selectedProductId = this.value;
        const selectedProduct = products.find(p => p.id === selectedProductId);
        populateVersionSelect(selectedProduct ? selectedProduct.versions : []);
    });
}

function populateVersionSelect(versions) {
    const versionSelect = document.getElementById('productVersion');
    // 既存のオプションをクリア
    versionSelect.innerHTML = '<option value="">選択してください</option>';
    versions.forEach(version => {
        const option = document.createElement('option');
        option.value = version;
        option.textContent = version;
        versionSelect.appendChild(option);
    });
}

document.getElementById('questionForm').addEventListener('submit', async function(event) {
    event.preventDefault();

    const submitButton = document.getElementById('submitButton');
    submitButton.disabled = true; // ボタンを無効化
    submitButton.textContent = '送信中...'; // ボタンテキストを変更

    const productIdInput = document.getElementById('productId');
    const productVersionInput = document.getElementById('productVersion');
    const noInput = document.getElementById('questionNo');
    let no = noInput.value.trim();
    const question = document.getElementById('question').value.trim();

    let isAutoAssigned = false;

    // 識別番号が空の場合は自動採番
    if (no === '') {
        isAutoAssigned = true;
        while (usedNos.includes(String(currentQuestionNo))) {
            currentQuestionNo++;
        }
        no = String(currentQuestionNo);
        noInput.value = no;
    }

    // 識別番号の重複チェック
    if (usedNos.includes(no)) {
        const messageDiv = document.getElementById('responseMessage');
        messageDiv.style.color = 'red';
        messageDiv.textContent = `エラー: 識別番号 "${no}" は既に使用されています。別の番号を入力してください。`;
        submitButton.disabled = false; // ボタンを再度有効化
        submitButton.textContent = '質問を登録'; // ボタンテキストを元に戻す
        return;
    }

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
            product_id: productIdInput.value.trim(),
            product_version: productVersionInput.value.trim(),
            sheet_id: sheet_id,
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
            if (!productIdInput.disabled && !productVersionInput.disabled) {
                disableFixedFields(productIdInput, productVersionInput);
            }
            // 使用済み識別番号リストに追加
            usedNos.push(no);
            // 自動採番された場合のみ次の番号を更新
            if (isAutoAssigned) {
                currentQuestionNo++;
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
function disableFixedFields(productIdInput, productVersionInput) {
    productIdInput.disabled = true;
    productVersionInput.disabled = true;
}

// エラーメッセージを表示する関数
function showErrorMessage(message) {
    const messageDiv = document.getElementById('responseMessage');
    messageDiv.style.color = 'red';
    messageDiv.textContent = message;
}

// ページロード時に製品データを初期化
document.addEventListener('DOMContentLoaded', initializeProductData);
