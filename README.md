# cavity code
## 2D cavity flow simulation using fractional step method 

キャビティ流れについては[カマキリさんのサイト](https://takun-physics.net/10217/)などで調べて下さい．  
ここでは各プログラムファイルについて，簡単に説明．  
（このプログラムには他人の著作物のコピーが含まれるので，多数に公開するときには注意して下さい．）  

## 使い方 1  
Fortran コードで実行 --> 出力されたFortranバイナリファイルを Pythonスクリプトで読み込み --> 流線を可視化．   

    $cp ./src/main.f90 ./run/

コンパイルと実行
    $cd ./run
    $gfortran main.f90
    $./a.out 

pythonスクリプトで可視化
    $cd ./script/ 
    $jupyter-notebook plot_fields.ipynb

## 使い方 2 
Pythonコードをjupyter-notebookで実行 (Fortranより計算が遅い)

    $cd ./python
    $jupyther-notebook 000_cavity_vel_pressure.ipynb

