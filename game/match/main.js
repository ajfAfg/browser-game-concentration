'use strict';
window.onload=function(){
  function Card(mark,figure){
    this.mark=mark;
    this.figure=figure;
    this.front;
    this.setFront=function(){
      this.front=`${this.mark}${('0'+this.figure).slice(-2)}.svg`;
    };
  }

  
  // カードを配列にセット
  const cards=[];
  // s=spade d=diamond h=heart c=club
  const marks=['s','d','h','c'];
  for(let i=0;i<marks.length;i++){
    for(let j=1;j<=13;j++){
      let card=new Card(marks[i],j);
      card.setFront();
      cards.push(card);
    }
  }

  // ランダム
  function shuffle(){
    let i=cards.length;
    while(i){
      let index=Math.floor(Math.random()*i--);
      var temp=cards[index];
      cards[index]=cards[i];
      cards[i]=temp;
    }
  }

  // トランプを配置
  shuffle();
  const table=document.getElementById('table');
  for(let i=0;i<marks.length;i++){
    let tr=document.createElement('tr');
    for(let j=0;j<13;j++){
      let td=document.createElement('td');
      let tempCard=cards[i*13+j];
      td.classList.add('card','back');
      td.onclick=flip;
      td.figure=tempCard.figure;
      td.style.backgroundImage=`url(images/${tempCard.front})`;
      tr.appendChild(td);
    }
    table.appendChild(tr);
  }


  // 待機時間のための変数
  function sleep(waitSec, callbackFunc) {
    // 経過時間（秒）
    var spanedSec=0;
    // 1秒間隔で無名関数を実行
    var id = setInterval(function () {
        spanedSec++;
        // 経過時間 >= 待機時間の場合、待機終了。
        if (spanedSec >= waitSec) {
            // タイマー停止
            clearInterval(id);
            // 完了時、コールバック関数を実行
            if (callbackFunc){
              callbackFunc();
            } 
        }
    }, 1200);
  }

  // 神経衰弱
  // 初期設定
  let firstCard=null;
  let playerMarker=0;
  var player1Point=0;
  var str1="プレイヤー1: "+player1Point+"枚";
  document.getElementById("player1_point").innerHTML = str1;
  var player2Point=0;
  var str2="プレイヤー2: "+player2Point+"枚";
  document.getElementById("player2_point").innerHTML = str2;
  document.getElementById("player").innerHTML = "プレイヤー1のターンです";

  function flip(e){
    let td=e.target;

    if(!td.classList.contains('back')){
      return;// 表のカードまたは待機時間中はをクリックしても何もしない。
    }
    td.classList.remove('back');// カードを表にする。
    if(firstCard===null){
      firstCard=td;// １枚目だったら今めくったカードをfirstCardに設定
    }else{
      // ２枚目だったら1枚目と比較して結果を判定する。
      if(firstCard.figure===td.figure){
        // ２枚が同じだったときの処理
        firstCard=null;
        if(playerMarker%2==0){
          player1Point=player1Point+2;
          var str1="プレイヤー1: "+player1Point+"枚";
          document.getElementById("player1_point").innerHTML = str1;
        }else{
          player2Point=player2Point+2;
          var str2="プレイヤー2: "+player2Point+"枚";
          document.getElementById("player2_point").innerHTML = str2;
        }
      }else{
        playerMarker++;
        sleep(1, function(){
          firstCard.classList.add('back');
          td.classList.add('back');
          firstCard=null;
        })
        if(playerMarker%2==0){
          document.getElementById("player").innerHTML="プレイヤー1のターンです";
        }else{
          document.getElementById("player").innerHTML="プレイヤー2のターンです";
        }; // ２枚目をめくった後待機時間を設けて見れるように
      };
    }
  }
}