pushd build-RelWithDebInfo-64
msbuild /m -p:Configuration=RelWithDebInfo INSTALL.vcxproj 
popd
