trash ../www/*
for filename in package-builders/*; do
	(cd "${filename}"; sh build.sh)
	rsync -r --exclude=".git/" "${filename}/build"/* ../www/
	##rsync -r "${filename}"/ build/
done
for filename in packages/*; do
	rsync -r --exclude=".git/" "${filename}"/* ../www/
	##rsync -r "${filename}"/ build/
done

