document.getElementById('state').textContent = 'Connecting...';

(async () => {
    const userId = generateUserId(32);
    sessionStorage.setItem('userId', userId);
    const url = location.protocol + '//' + location.hostname + ':8080/room';
    $.post(
        url,
        { 'user_id': userId },
        response => {
            console.log('res: ', response);
            if (response === 'false') {
                window.location.href = '../';
            } else {
                sessionStorage.setItem('matchingId', response);
                window.location.href = '../match/';
            }
        }
    );
})();

function generateUserId(n) {
    return btoa(String.fromCharCode(...crypto.getRandomValues(new Uint8Array(n)))).substring(0,n);
}
