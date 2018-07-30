## ゲームの目的(Purpose of the Game)
もちろん、 **思考を楽しむこと** です。
(Of course, to *enjoy thinking*.)

ルール的にいえば、あなたのカードを巧みに用いて **対戦相手のクリーチャーを全滅させる** ことが勝利条件になります。
(To say in terms of rules, to *destroy all opponents' creatures* is the win condition.)

## ゲームの要素(Elements of the Game)
もし **トライトレイン** に美麗なGUIクライアントが存在していれば、「百聞は一見にしかず」という手法もありえたのですが、残念ながら ~~まだ~~ 貧弱なCUIクライアントしか存在しません。
そのため、ゲームをよく理解していただくため、 **トライトレイン** の各要素を文章でなるべく簡単に解説します。
(If **TriTrain** had a beautiful GUI client, you could just run it and learn how to play.
However, there's only poor CUI client.
So you should understand how to play before playing.
In this section, we'll explain it in writing concisely.)

### カード(Cards)
**トライトレイン** はカードゲームと謳うだけあり、互いのプレイヤーが用意したカードがゲームの主役となります。
(Because **TriTrain** is a card game, cards are the leading part of the game.)

1枚のカードは、1体のクリーチャー(生物)を表します。
クリーチャーは次の3つの数値的ステータスを持ち、自発的に行動を起こす存在です。
(A card represents a creature in game.
Creatures, which spontaneously take actions, have three numeric statuses.)

* **HP** は、クリーチャーの生命力を表します。これが0になってしまうと、クリーチャーは「死亡」し、ゲームから退場することになります。(**HP** represents vitality. When it becomes 0, the creature "dies" and leaves the game.)
* **AT** は、クリーチャーの活動能力を表します。クリーチャーが行動を起こすときに、ATの値が大きいほど大きな効果をもたらすでしょう。(**AT** represents activeness. The larger AT is, the more that creature's actions affect others.)
* **AG** は、クリーチャーの素早さを表します。AGの値が大きいクリーチャーほど、先に行動を起こすことができます。(**AG** represents agility. Fast creatures can act before slow creatures.)

行動については後ほど、「行動フェイズ」の節で解説します。
(We will explain about "actions" in the Action Phase section below.)

### 盤面(Boards)
クリーチャーたちが激戦を繰り広げることになる主戦場、「盤面」について紹介します。
(Next, let's see boards, where creatures fight.)

まず2つの正三角形を想像してください。
三角形は左右に2つ並び、中央で2つの頂点が向かい合っている格好です。
(Imagine two regular triangles.
There is a left one and a right one, and two vertices face each other in the center.)

一方の正三角形の頂点3つが、そちら側のプレイヤーの「盤面」になります。
プレイヤーは基本的に、自分の盤面の頂点にカードを1枚ずつ配置して、ゲームを行います。
(Vertices of one triangle is the "board" of a player.
For each vertex, a player can put a card on it.)

盤面の名称を決めておきましょう。
真ん中にある2つの頂点を「 **前列** 」と呼びます。
ここに配置されたカードが戦闘の矢面に立つことになる、重要な位置です。
そこから反時計回りに、残りの2つの頂点をそれぞれ「左翼」、「右翼」と呼び、この2つのことはまとめて「 **後列** 」と呼びます。
(Now we will talk about the names of parts of the boards.
We call the center two vertices "forward".
It's an important position where the creature on it bears the brunt of the combat.
Call the other two vertices of the triangle respectively "left wing" and "right wing" in anti-clockwise.
"Backward" is the generic term of the two vertices.)

### デッキ(Decks)
**トライトレイン** でいう「デッキ」には2つの意味があります。
(The term "deck" has two meanings in **TriTrain**.)

* ゲームの参加者が持ちよる、カードの束。(A sheaf of cards players prepared.)
* ゲーム中に、カードが配置される領域の一種。(An area in game where cards are stacked face down in game.)

1つ目の「デッキ」はあくまでゲームが始まる前の話だということに注意しますか？
いえ、あまり言葉の話ばかりしてもアレですし、本題に戻りましょう。
(It's useful to know that the first meaning of deck isn't actually used in game.
Ah, yes, it's no use to discuss terminology, so back to explanation.)

**トライトレイン** の1つのデッキは、「ちょうど7枚のカード」からなります。
もっとも重要な点は、「カードの順番はプレイヤーが自由に決められる」ことです。
これについては、デッキ作りの話をする際にとっておきましょう。
(A deck of **TriTrain** consists of exactly seven cards.
The most important point here is that players may choose the order of their deck.
We'll explain this more when explaining how to build decks.)

2つ目の「デッキ」は、ゲーム中にカードが(裏向きで)積まれる場所です。
Magic: the Gathering でいうところのライブラリーにあたります。
ゲームの開始と同時に、1つ目の意味でのデッキが、そのまま2つ目の意味でのデッキになる、という言い方をすれば興味深く感じるかもしれません。
(A deck of the second meaning is the area in game where cards are stack faced down.
In Magic: the Gathering, it's called a "library".
It maybe sounds interesting that a deck of the first meaning becomes a deck of the second meaning as the game begins.)

### ターン(Turns)
**トライトレイン** のゲームはターン制です。
1ターンの流れは単純で、次のようになります。
(**TriTrain**'s game is turn-based.
The flow of a turn is:)

0. 召喚フェイズ(Summon Phase)
0. ????フェイズ(???? Phase)
0. 行動フェイズ(Act Phase)
0. 回転フェイズ(Rotate Phase)
0. ターン経過フェイズ(Pass Phase)

#### 召喚フェイズ(Summon Phase)
これまでの説明で分かる通り、ゲームの開始時点では、デッキにカードが積まれているだけで、盤面は空っぽです。
そこで、召喚フェイズです。
(As you may be aware, at the beginning of the game, the board is empty.
Then, the Summon Phase.)

* プレイヤーの盤面に空(クウ)の頂点があり、デッキにカードがあるなら、デッキの一番上のカードをその頂点に配置します。(If there's an empty vertex in a player's board and some cards are in that player's deck, put the top card of the deck onto that vertex.)
* 空の頂点が複数あるなら、「反時計回り」に配置していく。(If there're multiple empty vertices, put that many cards down simultaneously in anti-clockwise.)

デッキにカードがなく、盤面が空っぽのままだったら？
冒頭で述べたとおり、残念ながら対戦相手の勝利に終わります。
(What if your board is empty but your deck contains no cards?
As we said at the beginning, unfortunately you lose the game.)

#### ????フェイズ(???? Phase)
おっと、この隠されたフェイズについては、実際にゲームを遊んだときのお楽しみです。
(You will see that the secret phase is when you play the game.)

#### 行動フェイズ(Act Phase)
**トライトレイン** のメインとなるフェイズです。
このフェイズでは、盤面に出ているクリーチャー (最大6体) が、AG (素早さ) の高い順に1回ずつ「行動」を起こします。
(This is the main phase of the game.
In this phase, each of creatures on the board (up to six) takes an action in descending order of AGs (agilities).)

クリーチャーは基本的に2種類の行動を持ちます。「 **前列行動** 」と「 **後列行動** 」です。
その名の通り、前列行動は「クリーチャーが前列にいる起こす行動」であり、後列行動は「クリーチャーが後列にいるときに起こす行動」となります。
(Creatures basically have two types of actions: *Forward actions* and *backward actions*.
As the names describes, creatures take their forward action when it's on forward and take their backward action on backward.)

行動にはいろいろあります。
相手のクリーチャーにダメージを与えて HP を減らし、死亡させようとするもの。
あるいは、クリーチャーの AT や AG を変化させるもの、などなど。
詳しくは [効果](効果(Effects)) と [プリセット](プリセット(Preset)) をご覧ください。
(The kinds of actions are various.
One to deal damage to opponent's creatures in order to kill them, one to change creature's ATs and AGs, and so on.
See [Effects](効果(Effects)) and [Preset](プリセット(Preset)) for details.)

#### 回転フェイズ(Rotate Phase)
行動フェイズでは、クリーチャーが死亡することがあります。
そして、死亡したクリーチャーは、即座にゲームから退場します。
(ええ、ちょっと語弊がありますが、それも後述です。)
(In the Act Phase, creatures may possibly die.
And when a creature dies, it leaves the game immediately.)

そういうわけで、このフェイズに来たときには、盤面に空の頂点があります。
回転フェイズは、クリーチャーを前に移動させることで、空の頂点を「後ろ」に集めるフェイズです。
(So at the beginning of this phase, some vertices may be empty.)

* 盤面にいるすべてのクリーチャーを、反時計回りにできるだけ移動させる。(Rotate creatures anti-clockwise when possible.)

これにより、クリーチャーは前へ前へと押し出されます。
最終的には戦闘の矢面である「前列」に立たされ、そして散っていくのでしょう。
(By this, creatures go forward gradually.
Finally, creatures will be made to stand on a forward vertex and die because of their opponent's attacks.)

#### ターン経過フェイズ(Pass Phase)
これは読んで字のごとく、ターンの経過を表すフェイズです。
例えば「2ターンの間、～という効果を得る。」といった効果の経過ターン数が更新されたりします。
(This is the phase to pass the turn, as the name describes.
For example, effects like "A creature gains an effect for two turns." update their count of past turns.)

ちなみに、 **トライトレイン** は仕様上、完全にゲームがループすることがあります。
そのような場合に対処するため、20ターン目が終了した時点で、ゲームは引き分けになります。
(In passing, the game of **TriTrain** possibly loops.
To deal with it, the game ends in a draw at the end of the 20th turn.)

## 継続(Continuation)
まだ解説していないこともいろいろありますが、以上のことを把握しておけばひとまず大丈夫でしょう。
(Although there are many things we haven't explained yet, it's okay to know only these things for the beginners.)

さて、あなたが次にやるべきことは、あなただけのカードとデッキを作ること、ですね。
そのためには、[カードとデッキの制作](カードとデッキの制作(Making_cards_and_decks)) をぜひご一読ください。
(Now what you want to do next is... to make your cards and decks, no?
Please read [Making cards and decks](カードとデッキの制作(Making_cards_and_decks)).)