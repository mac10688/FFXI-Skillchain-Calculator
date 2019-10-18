set -e

function makeHtml {
  cat <<EOF > $1
<!DOCTYPE HTML>
<html>

<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>$2</title>
</head>

<body>

<script type="text/javascript">
$(cat $3)
var app = Elm.Main.init({
  flags: {
    width: window.innerWidth,
    height: window.innerHeight
  }
});
</script>

</body>
</html>
EOF

}

rm -rf _site

mkdir -p _site
cp -r static/ _site/

## GENERATE CSS


for sass in $(find css/sass -type f -name "*.sass")
do
  subpath="${sass#css/sass}"
  name="${subpath%.sass}"

  css="_site/css/$name.css"

  mkdir -p $(dirname $css)

  echo "Creating: $css"

  sass $sass $css

done

for css in $(find css -type f -name "*.css")
do
  subpath="${css#css}"
  name="${subpath%.css}"
  echo "Copying: $css"
  cp $css _site/css/
done

## GENERATE HTML

mkdir -p _temp

for elm in $(find src/pages -type f -name "*.elm")
do
    subpath="${elm#src/pages/}"
    name="${subpath%.elm}"

    js="_temp/$name.js"
    html="_site/$name.html"

    mkdir -p $(dirname $js)
    mkdir -p $(dirname $html)

    #if [ -f $html ] && [ $(date -r $elm +%s) -eq $(date -r $html +%s) ]
    #then
        #echo "Cached: $elm"
    #else
        echo "Compiling: $elm"
        rm -f elm-stuff/*/Main.elm*
        elm make $elm --optimize --output=$js > /dev/null
        #uglifyjs $js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
        #  | uglifyjs --mangle \
        #  | makeHtml $html $name
        makeHtml $html $name $js
        touch -r $elm $html
    #fi
done

rm -rf _temp
