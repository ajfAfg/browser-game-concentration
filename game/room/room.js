document.getElementById('state').textContent = 'Connecting...';

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

$.post(location.protocol + '//' + location.hostname + ':8080/room',
    {'user_id' : '1'},
    (dt) => alert(dt)
);
