# cavity code (演習I 2020年度版)
## 2D cavity flow simulation using fractional step method 

キャビティ流れについては[カマキリさんのサイト](https://takun-physics.net/10217/)などで調べて下さい．  
ここでは各プログラムファイルについて，簡単に説明．  
現状のプログラムは以下を参考にしました。他人の著作物のコピーが一部含まれるので， public公開できません.  
 (参考)「暗黙の型宣言」 https://note.com/implicit_none

以下の前提となるスキルは自習  
- Linuxコマンドの基礎 (cd, mkdir, ls, cp, rm など)
- エディター(Vim, Emacs, Atomなど)の使い方 
- Fortranでのコンパイルなど 

とりあえずpythonのみで動かして見たい人は, 使い方2へ　　


## 使い方 1  
./run フォルダ内でFortranコードをコンパイル・実行  --> ./pool/ に出力されたFortranバイナリファイルをPythonスクリプトで読み込み  --> 流線を可視化．   
という手順です．

1. まずソースプログラムのコピーをrunフォルダにコピー
```    
    $cp ./src/main.f90 ./run/
    $mkdir ./run/pool/ 
```
2. コンパイルと実行 (main.f90の中にパラメターが書かれているので， パラメターを変更するたびにコンパイルして， 実行する． ./pool/フォルダに書き出されます． ./poolフォルダを作成していない場合はエラーが出ます．上書き注意．)
```
    $cd ./run
    $gfortran main.f90
    $./a.out
```
3. pythonスクリプトで可視化 (2の実行時に出力される時系列ファイルをpythonで読み込んでいます． エンディアン(littleかbig)とFortran特有のバイトレコードマーカーに注意します． また， データは倍精度(8byte)で書き出されています．)
```
    $cd ./scripts/
    $jupyter-notebook plot_fields.ipynb
```
## 使い方 2 (Fortranやコマンド操作を使わずにとりあえず実行してみたい人向け) 
000_cavity_vel_pressure.ipynb を jupyter-notebookで実行してください(Fortranより計算が遅い).    
ターミナルからは以下．
```
    $cd ./python
    $jupyter-notebook 000_cavity_vel_pressure.ipynb
```
