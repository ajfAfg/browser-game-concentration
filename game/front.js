(() => {
    sessionStorage.removeItem('matchingId');
    if (sessionStorage.getItem('userId') === null) {
        sessionStorage.setItem('userId', generateUserId(32) );
    }

    function generateUserId(n) {
        return btoa(String.fromCharCode(...crypto.getRandomValues(new Uint8Array(n)))).substring(0,n);
    }
})();
