document.getElementById('js-state').textContent = 'Connecting...';

(() => {
    const userId = sessionStorage.getItem('userId');
    if (userId === null) {
        window.location.href = '../';
    } else {
        connectRoomAsync(userId);
    }

    async function connectRoomAsync(userId) {
        const url = location.protocol + '//' + location.hostname + ':8080/room';
        const data = {
            'user_id': userId
        };
        $.post(url, data, callback);
    
        function callback(response) {
            if (response === 'false') {
                window.location.href = '../';
            } else {
                sessionStorage.setItem('matchingId', response);
                window.location.href = '../match/';
            }
        }
    }
})();
