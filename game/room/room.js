document.getElementById('state').textContent = 'Connecting...';

/*
(() => {
    const waitTime = 60000;
    const startTime = Date.now();
    
    const timeId = setInterval(() => {
        const diff = waitTime - (Date.now() - startTime);
        const remain = Math.ceil(diff / 1000);
    
        if (remain <= 0) {
            clearInterval(timeId);
            window.location.href = '../';
        }
        document.querySelector('#timer').innerHTML = `残り${remain}秒`;
    });
})();
*/
(async () => {
    const userId = generateUserId(32);
    sessionStorage.setItem('userId', userId);
    $.post(location.protocol + '//' + location.hostname + ':8080/room',
        { 'user_id': userId },
        (matchingId) => {
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
