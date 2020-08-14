# cavity code
## 2D cavity flow simulation using fractional step method 

キャビティ流れについては[カマキリさんのサイト](https://takun-physics.net/10217/)などで調べて下さい．
ここでは各プログラムファイルについて，簡単に説明．
（このプログラムには他人の著作物のコピーが含まれるので，多数に公開するときには注意して下さい．）

## 使い方 1  
Fortran コードで実行 --> 出力されたFortranバイナリファイルを Pythonスクリプトで読み込み --> 流線を可視化． 
'./src/main.f90' 
Fortran メインファイル

    $gfortran main.f90
    $./a.out 

## 使い方 2 
Python　コードを実行 (Fortranより計算が遅い)
