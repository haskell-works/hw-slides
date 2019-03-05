Reveal.addEventListener('ready', function(event) {
  var draw = SVG('drawing').size(300, 300)
  var nested = draw
  var rect = nested.rect(100, 100).attr({ fill: '#606' })
  nested.rect(50, 50).attr({fill: '#088'})
});
