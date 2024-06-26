var db;

function load_db(gb){
  var script = document.createElement("script");
  script.src = "tracksdb.js";
  document.getElementsByTagName('head')[0].appendChild(script);
  script.onload = function(){
    var binaryString = atob(tracksdb);
    var bytes = new Uint8Array(binaryString.length);
    for (var i = 0; i < binaryString.length; i++) {
      bytes[i] = binaryString.charCodeAt(i);
    }
    db = new SQL.Database(bytes);
    gb();
  }
}

function get_query(sql,callback){
  var res = db.exec(sql);
  res = (res.length == 0)? false : res[0].values;
  callback(res);
}

function get_tracks(chr, start, end){
    db.run("DROP TABLE IF EXISTS current_region");
    db.run("CREATE TEMP TABLE current_region AS SELECT rowid,* FROM tbl_segments WHERE chr='"+chr+"' AND (start BETWEEN "+start+" AND "+end+" OR end BETWEEN "+start+" AND "+end+" OR "+start+" BETWEEN start AND end OR "+end+" BETWEEN start AND end)");
    var res = {};
    var results = db.exec("SELECT trackid,count(trackid) FROM current_region GROUP BY trackid");
    if(results[0]){
      results[0].values.forEach(function(row){
        if(row[1]>1000)
          res['track'+row[0]] = [[0,row[0],false]];
        else
          res['track'+row[0]] = db.exec("SELECT * FROM current_region WHERE trackid="+row[0]+" ORDER BY start")[0].values;
      });
    }
    return res;
}

function get_dna(d,callback){

  var chr = d[2],
  start = d[3],
  end = d[4],
  name = d[5],
  strand = d[7],
  thickStart = d[8],
  thickEnd = d[9],
  blockCount = d[11],
  blockSizes = d[12],
  blockStarts = d[13];

  var i,
  thickSize = thickEnd - thickStart,
  data = {};

  var js = document.createElement("script");
  js.src = "sequences/"+chr+".js";
  document.getElementsByTagName('head')[0].appendChild(js);

  js.onload = function(e) {
    var dna = sequence.slice(start,end).toLowerCase();

    if(blockCount){
      sizes = blockSizes.split(',');
      starts = blockStarts.split(',');
      var clean = "";
      for(i = 0; i < dna.length ; i++)
        clean = clean + "N";
      for(i = 0; i < blockCount; ++i)
        clean = substr_replace(clean,dna.substr(+starts[i],+sizes[i]),+starts[i],+sizes[i]);
      dna = clean;
    }

    if(strand && strand == '-'){
      var rev = "";
      for(i = dna.length-1; i >= 0; --i) {
        switch (dna[i]) {
          case 'a':
            rev = rev+'t';
            break;
          case 't':
            rev = rev+'a';
            break;
          case 'g':
            rev = rev+'c';
            break;
          case 'c':
            rev = rev+'g';
            break;
          case 'N':
            rev = rev+'N';
            break;
        }
      }
      dna = rev;
      thickStart = end-thickEnd;
    }else{
      thickStart = thickStart-start;
    }

    if(name){
      var dnaProt = '';
      if(thickStart&&thickEnd){
        dnaProt = dna.substr(thickStart,thickSize);
      }else{
        dnaProt = dna;
      }
      if(blockCount){
        dnaProt = dnaProt.replace(/N/g,"");
        dna = dna.replace(/N/g,"");
      }
      var prot = "",
          cod = "";
      for(i = 0; i < dnaProt.length; i = i+3){
        cod = dnaProt.substr(i, 3);
        switch (cod){
              case 'gct':
              case 'gcc':
              case 'gca':
              case 'gcg':
                cod = 'A';
                break;
              case 'cgt':
              case 'cgc':
              case 'cga':
              case 'cgg':
              case 'aga':
              case 'agg':
                cod = 'R';
                break;
              case 'aat':
              case 'aac':
                cod = 'N';
                break;
              case 'gat':
              case 'gac':
                cod = 'D';
                break;
              case 'tgt':
              case 'tgc':
                cod = 'C';
                break;
              case 'gaa':
              case 'gag':
                cod = 'E';
                break;
              case 'caa':
              case 'cag':
                cod = 'Q';
                break;
              case 'ggt':
              case 'ggc':
              case 'gga':
              case 'ggg':
                cod = 'G';
                break;
              case 'cat':
              case 'cac':
                cod = 'H';
                break;
              case 'att':
              case 'atc':
              case 'ata':
                cod = 'I';
                break;
              case 'tta':
              case 'ttg':
              case 'ctt':
              case 'ctc':
              case 'cta':
              case 'ctg':
                cod = 'L';
                break;
              case 'aaa':
              case 'aag':
                cod = 'K';
                break;
              case 'atg':
                cod = 'M';
                break;
              case 'ttt':
              case 'ttc':
                cod = 'F';
                break;
              case 'cct':
              case 'ccc':
              case 'cca':
              case 'ccg':
                cod = 'P';
                break;
              case 'tct':
              case 'tcc':
              case 'tca':
              case 'tcg':
              case 'agt':
              case 'agc':
                cod = 'S';
                break;
              case 'act':
              case 'acc':
              case 'aca':
              case 'acg':
                cod = 'T';
                break;
              case 'tgg':
                cod = 'W';
                break;
              case 'tat':
              case 'tac':
                cod = 'Y';
                break;
              case 'gtt':
              case 'gtc':
              case 'gta':
              case 'gtg':
                cod = 'V';
                break;
              case 'taa':
              case 'tag':
              case 'tga':
                cod = '*';
                break;
              default:
                cod = 'X';
        }
        prot = prot+cod;
      }
      for(i = 50; i < prot.length; i = i+51)
        prot = substr_replace(prot,"\n",i,0);
      data.prot = prot;
    }

    for(i = 50; i < dna.length; i = i+51)
      dna = substr_replace(dna,"\n",i,0);
    data.dna = dna;

    callback(false, data);
  }

  js.onerror = function () {
    callback(false, false);
  }

  function substr_replace(str,rep,i,len) {
    var before = str.substring(0,i),
        after = str.substring(i+len);
    return before + rep + after;
  }
}
