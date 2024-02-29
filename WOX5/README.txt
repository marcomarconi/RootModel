# I used the following srun batch

dir=$1;
mdx=$2;
param1="Auxin Source"
param2="WOX5 Basal Production Rate"
param3="WOX5 Induction by Auxin"
param4="Substrate Fixed"
snapsP="Snapshots Directory"
default1="10"
default2="0"
default3="1"
default4="True"
snapsD="Test"
rm -rf $dir
mkdir $dir;
cp usrLibRoot.so $dir
cp $mdx".mdxm" $dir
cp $0 $dir
for i in `seq 1 9 100`; do
 for j in `seq 0.0 0.02 0.1`; do
 for k in 0.001 1.0 100.0  ; do
 #  for l in True False; do
    echo $i $j $k $l    
    combo=$i"_"$j"_"$k"_"$l"_"$p
    file=test.$combo.mdxv
    cp $mdx".mdxv"  $dir/$file;
    cd $dir;
    
    sed -e ':a' -e 'N' -e '$!ba' -e 's/ParmName: '"$param1"'\nParmString: '$default1'/ParmName: '"$param1"'\nParmString: '$i'/' -i  $file;
    sed -e ':a' -e 'N' -e '$!ba' -e 's/ParmName: '"$param2"'\nParmString: '$default2'/ParmName: '"$param2"'\nParmString: '$j'/' -i  $file;
    sed -e ':a' -e 'N' -e '$!ba' -e 's/ParmName: '"$param3"'\nParmString: '$default3'/ParmName: '"$param3"'\nParmString: '$k'/' -i  $file;
  #  sed -e ':a' -e 'N' -e '$!ba' -e 's/ParmName: '"$param4"'\nParmString: '$default4'/ParmName: '"$param4"'\nParmString: '$l'/' -i  $file;
 
    sed -e ':a' -e 'N' -e '$!ba' -e 's/ParmName: '"$snapsP"'\nParmString: '$snapsD'/ParmName: '"$snapsP"'\nParmString: Test.'$combo'/' -i   $file;
    sed -e ':a' -e 'N' -e '$!ba' -e 's/ParmName: Output Mesh\nParmString: mesh.mdxm/ParmName: Output Mesh\nParmString: mesh.'$combo'.mdxm/' -i $file;
    sed -e ':a' -e 'N' -e '$!ba' -e 's/ParmName: Debug File\nParmString: debug.csv/ParmName: Debug File\nParmString: debug.'$combo'.csv/' -i $file;
    srun -t 120 -p fast singularity exec ~/mdx2.sif xvfb-run -a -s "-screen 0 1920x1080x24"  MorphoDynamX  '--model' 'Model/Root/01 Root' '--addlibrary' 'usrLibRoot.so' $file --run > output.$combo 2> err.$combo &
    sleep 0.1
    cd ..;
   done;
  done;
 done;
#done


