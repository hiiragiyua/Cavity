# cavity code
## 2D cavity flow simulation using fractional step method 

キャビティ流れについては[カマキリさんのサイト](https://takun-physics.net/10217/)などで調べて下さい．  
ここでは各プログラムファイルについて，簡単に説明．  
（このプログラムには他人の著作物のコピーが含まれるので，多数に公開するときには注意して下さい．）  

## 使い方 1  
Fortranで実行  --> 出力されたFortranバイナリファイルをPythonスクリプトで読み込み  --> 流線を可視化．   
という手順です．

1. まずソースプログラムのコピーをrunフォルダにコピー
```    
    $cp ./src/main.f90 ./run/
```
2. コンパイルと実行 (main.f90の中にパラメターが書かれているので， パラメターを変更するたびにコンパイルして， 実行する．)
```
    $cd ./run
    $gfortran main.f90
    $./a.out
```
3. pythonスクリプトで可視化 (2の実行時に出力される時系列ファイルをpythonで読み込んでいます． エンディアン(littleかbig)とFortran特有のバイトレコードマーカーに注意します． また， データは倍精度(8byte)で書き出されています．)
```
    $cd ./script/
    $jupyter-notebook plot_fields.ipynb
```
## 使い方 2 (Fortranを使わずにとりあえず実行してみたい人向け) 
Pythonコードをjupyter-notebookで実行 (Fortranより計算が遅い)
```
    $cd ./python
    $jupyther-notebook 000_cavity_vel_pressure.ipynb
```
