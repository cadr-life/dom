// jscl xhr-receive
var reqXHRsendNull = function (req) {
      req.send(null);
}



// master change
// (#j:opNew #j:window "XMLHttpRequest")
var opNew =  function () {
   var args = [].concat(null,Array.prototype.slice.call(arguments,2));
   var fn = arguments[0][arguments[1]];
   return new (Function.prototype.bind.apply(fn,args))();
   };


// master change
// (#j:opEval "var dd = {}")
var opEval = function (s) { 
     var res = window.eval(s); 
     if (res === undefined) { 
            res = "undefined"; } 
     return res; 
    };

