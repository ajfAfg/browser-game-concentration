document.getElementById('state').textContent = 'Connecting...';

(async () => {
    const userId = generateUserId(32);
    sessionStorage.setItem('userId', userId);
    const url = location.protocol + '//' + location.hostname + ':8080/room';
    $.post(
        url,
        { 'user_id': userId },
        matchingId => {
            if (matchingId === 'no_matching') {
                window.location.href = '../';
            } else {
                sessionStorage.setItem('matchingId', matchingId);
                window.location.href = '../match/';
            }
        }
    );
})();

function generateUserId(n) {
    return btoa(String.fromCharCode(...crypto.getRandomValues(new Uint8Array(n)))).substring(0,n);
}
