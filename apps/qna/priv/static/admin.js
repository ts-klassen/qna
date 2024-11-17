// このファイルはindex.html内にインラインで含まれています。
// 外部ファイルとして管理したい場合は、以下のスクリプトタグを使用して読み込んでください。
// <script src="admin.js"></script>

// 基本認証情報を設定
const username = prompt("管理者ユーザー名を入力してください:");
const password = prompt("管理者パスワードを入力してください:");
const authHeader = 'Basic ' + btoa(username + ':' + password);

// ユーザー一覧の取得と表示
function fetchUsers() {
    fetch('/qna/api/v1/users', {
        method: 'GET',
        headers: {
            'Authorization': authHeader
        }
    })
    .then(response => {
        if (response.status === 200) {
            return response.json();
        } else {
            alert('ユーザー情報の取得に失敗しました。');
        }
    })
    .then(data => {
        if (data && data.success) {
            const tbody = document.querySelector('#users-table tbody');
            tbody.innerHTML = '';
            data.users.forEach(user => {
                const tr = document.createElement('tr');
                tr.innerHTML = `
                    <td>${user.id}</td>
                    <td>${user.is_admin ? 'はい' : 'いいえ'}</td>
                    <td>
                        <button onclick="changePassword('${user.id}')">パスワード変更</button>
                    </td>
                `;
                tbody.appendChild(tr);
            });
        }
    })
    .catch(error => {
        console.error('Error fetching users:', error);
    });
}

// 新規ユーザーの追加
document.getElementById('add-user-form').addEventListener('submit', function(e) {
    e.preventDefault();
    const userId = document.getElementById('new-user-id').value;
    const isAdmin = document.getElementById('new-user-is-admin').checked;

    fetch('/qna/api/v1/users', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Authorization': authHeader
        },
        body: JSON.stringify({
            id: userId,
            is_admin: isAdmin
        })
    })
    .then(response => response.json())
    .then(data => {
        if (data.success) {
            alert(`ユーザーが追加されました。パスワード: ${data.passwd}`);
            fetchUsers();
            document.getElementById('add-user-form').reset();
        } else {
            alert('ユーザーの追加に失敗しました。');
        }
    })
    .catch(error => {
        console.error('Error adding user:', error);
    });
});

// パスワード変更
function changePassword(userId) {
    const newPassword = prompt(`ユーザー "${userId}" の新しいパスワードを入力してください:`);
    if (newPassword) {
        fetch('/qna/api/v1/users/passwd', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
                'Authorization': authHeader
            },
            body: JSON.stringify({
                id: userId,
                passwd: newPassword
            })
        })
        .then(response => response.json())
        .then(data => {
            if (data.success) {
                alert('パスワードが更新されました。');
            } else {
                alert('パスワードの更新に失敗しました。');
            }
        })
        .catch(error => {
            console.error('Error changing password:', error);
        });
    }
}

// IPアドレス一覧の取得と表示
function fetchIPs() {
    fetch('/qna/api/v1/ip', {
        method: 'GET',
        headers: {
            'Authorization': authHeader
        }
    })
    .then(response => {
        if (response.status === 200) {
            return response.json();
        } else {
            alert('IPアドレス情報の取得に失敗しました。');
        }
    })
    .then(data => {
        if (data && data.success) {
            const tbody = document.querySelector('#ips-table tbody');
            tbody.innerHTML = '';
            data.ip_list.forEach(ip => {
                const tr = document.createElement('tr');
                tr.innerHTML = `
                    <td>${ip.ip}</td>
                    <td>${ip.memo || ''}</td>
                `;
                tbody.appendChild(tr);
            });
        }
    })
    .catch(error => {
        console.error('Error fetching IPs:', error);
    });
}

// 新規IPアドレスの追加
document.getElementById('add-ip-form').addEventListener('submit', function(e) {
    e.preventDefault();
    const ip = document.getElementById('new-ip-address').value;
    const memo = document.getElementById('new-ip-memo').value;

    fetch('/qna/api/v1/ip', {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json',
            'Authorization': authHeader
        },
        body: JSON.stringify({
            ip: ip,
            memo: memo
        })
    })
    .then(response => response.json())
    .then(data => {
        if (data.success) {
            alert('IPアドレスが追加されました。');
            fetchIPs();
            document.getElementById('add-ip-form').reset();
        } else {
            alert('IPアドレスの追加に失敗しました。');
        }
    })
    .catch(error => {
        console.error('Error adding IP address:', error);
    });
});

// ページロード時にデータを取得
window.onload = function() {
    fetchUsers();
    fetchIPs();
};
