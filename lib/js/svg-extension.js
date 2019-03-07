SVG.extend(SVG.Element, {
  with: function(f) {
    f(this)
    return this
  }
})

SVG.extend(SVG.Text, SVG.Tspan, {
  nl: function() {
    return this.tspan('​').newLine()
  },
  tspans: function(text, keys, styles) {
    if (styles === undefined) {
      styles = {}
    }
    
    var i = 0;
    for (; i < text.length; ++i) {
      this.tspan(text[i]).with(e => {
        if (keys[i] in styles) {
          e.addClass(styles[keys[i]])
        }
      })

    }
    return this
  }
})

// Reveal.addEventListener('ready', function(event) {
//   var draw = SVG('drawing').size(300, 300)
//   var nested = draw
//   var rect = nested.rect(100, 100).attr({ fill: '#606' })
//   nested.rect(50, 50).attr({fill: '#088'})
// });

