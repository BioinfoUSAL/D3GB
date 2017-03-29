<?php

class MyDB extends SQLite3 {
  private $send;

  function __construct() {
    $this->open('Tracks.db');
    $this->busyTimeout(1000);
    $this->send = array();
  }

  public function load_query($q){
    $results = $this->query($q);
    while ($row = $results->fetchArray(SQLITE3_NUM)){
      $this->send[] = $row;
    }
  }

  public function load_tracks($region,$rowid){
    $tracks = $this->query("SELECT trackid,count(trackid) ".$region." GROUP BY trackid");
    while ($track = $tracks->fetchArray(SQLITE3_NUM)){
      if($track[1]!=0){
        if($track[1]>1000){
          $this->send['track'.$track[0]] = [[0,$track[0],FALSE]];
        }else{
          $res = $this->query("SELECT $rowid* $region AND trackid=".$track[0]." ORDER BY start");
          $this->send['track'.$track[0]] = [];
          while ($row = $res->fetchArray(SQLITE3_NUM)){
            $this->send['track'.$track[0]][] = $row;
          }
        }
      }
    }
  }

  public function get_send(){
    return json_encode($this->send);
  }
}

if(isset($_POST["q"])){
  $q = $_POST["q"];

  $db = new MyDB();

  $db->load_query($q);

  echo $db->get_send();
  $db->close();
}

if(isset($_POST["r"])){
  $aux = location_decode($_POST["r"]);
  $chr = $aux[0];
  $start = $aux[1];
  $end = $aux[2];

  $db = new MyDB();

  $region = "FROM tbl_segments WHERE chr='".$chr."' AND (start BETWEEN ".$start." AND ".$end." OR end BETWEEN ".$start." AND ".$end." OR ".$start." BETWEEN start AND end OR ".$end." BETWEEN start AND end)";
  $db->exec("CREATE TEMP TABLE current_region AS SELECT rowid,* ".$region." LIMIT 6000");
  $rowid = "rowid,";

  $count = $db->query("SELECT count(*) from current_region");
  if($count->fetchArray(SQLITE3_NUM)[0]<6000){
    $region = "FROM current_region WHERE 1";
    $rowid = "";
  }

  $db->load_tracks($region,$rowid);

  echo $db->get_send();
  $db->close();
}

if(isset($_GET["r"])){
  $response = array();
  $aux = location_decode($_GET["r"]);
  $chr = $aux[0];
  $start = $aux[1];
  $end = $aux[2];
  $Fa = "sequences/".$chr.".fa";
  $handle = fopen($Fa,"r");
  if($handle){
    fgets($handle);
    fseek($handle,$start,SEEK_CUR);
    $dna = fread($handle,($end-$start));
    fclose($handle);
    $dna = strtolower($dna);

    if(isset($_GET["thickStart"])&&isset($_GET["thickEnd"])){
      $thickStart = intval($_GET["thickStart"]);
      $thickEnd = intval($_GET["thickEnd"]);
      $thickSize = $thickEnd-$thickStart;
    }else{
      $thickStart = $start;
      $thickEnd = $end;
    }

    if(isset($_GET["blockCount"])){
      $count = intval($_GET["blockCount"]);
      $sizes = explode(',',$_GET["blockSizes"]);
      $starts = explode(',',$_GET["blockStarts"]);
      $clean = "N";
      $clean = str_pad($clean, strlen($dna),"N");
      for($i = 0; $i < $count; ++$i) {
        $clean = substr_replace($clean,substr($dna,$starts[$i],$sizes[$i]),$starts[$i],$sizes[$i]);
      }
      $dna = $clean;
    }

    if(isset($_GET["strand"])){
      $reverse = "";
      for($i = strlen($dna)-1; $i >= 0; --$i) {
        switch ($dna[$i]) {
          case 'a':
            $reverse = $reverse.'t';
            break;
          case 't':
            $reverse = $reverse.'a';
            break;
          case 'g':
            $reverse = $reverse.'c';
            break;
          case 'c':
            $reverse = $reverse.'g';
            break;
          case 'N':
            $reverse = $reverse.'N';
            break;
        }
      }
      $dna = $reverse;
      $thickStart = $end-$thickEnd;
    }else{
      $thickStart = $thickStart-$start;
    }

    if(isset($_GET["name"])){
      if(isset($_GET["thickStart"])&&isset($_GET["thickEnd"])){
        $dnaProt = substr($dna,$thickStart,$thickSize);
      }else{
        $dnaProt = $dna;
      }
      if(isset($_GET["blockCount"])){
        $dnaProt = str_replace("N","",$dnaProt);
        $dna = str_replace("N","",$dna);
      }
      $prot = "";
      for($i = 0; $i < strlen($dnaProt); $i = $i+3){
        $cod = substr($dnaProt, $i, 3);
        switch ($cod){
              case 'gct':
              case 'gcc':
              case 'gca':
              case 'gcg':
                $cod = 'A';
                break;
              case 'cgt':
              case 'cgc':
              case 'cga':
              case 'cgg':
              case 'aga':
              case 'agg':
                $cod = 'R';
                break;
              case 'aat':
              case 'aac':
                $cod = 'N';
                break;
              case 'gat':
              case 'gac':
                $cod = 'D';
                break;
              case 'tgt':
              case 'tgc':
                $cod = 'C';
                break;
              case 'gaa':
              case 'gag':
                $cod = 'E';
                break;
              case 'caa':
              case 'cag':
                $cod = 'Q';
                break;
              case 'ggt':
              case 'ggc':
              case 'gga':
              case 'ggg':
                $cod = 'G';
                break;
              case 'cat':
              case 'cac':
                $cod = 'H';
                break;
              case 'att':
              case 'atc':
              case 'ata':
                $cod = 'I';
                break;
              case 'tta':
              case 'ttg':
              case 'ctt':
              case 'ctc':
              case 'cta':
              case 'ctg':
                $cod = 'L';
                break;
              case 'aaa':
              case 'aag':
                $cod = 'K';
                break;
              case 'atg':
                $cod = 'M';
                break;
              case 'ttt':
              case 'ttc':
                $cod = 'F';
                break;
              case 'cct':
              case 'ccc':
              case 'cca':
              case 'ccg':
                $cod = 'P';
                break;
              case 'tct':
              case 'tcc':
              case 'tca':
              case 'tcg':
              case 'agt':
              case 'agc':
                $cod = 'S';
                break;
              case 'act':
              case 'acc':
              case 'aca':
              case 'acg':
                $cod = 'T';
                break;
              case 'tgg':
                $cod = 'W';
                break;
              case 'tat':
              case 'tac':
                $cod = 'Y';
                break;
              case 'gtt':
              case 'gtc':
              case 'gta':
              case 'gtg':
                $cod = 'V';
                break;
              case 'taa':
              case 'tag':
              case 'tga':
                $cod = '*';
                break;
              default:
                $cod = 'X';
        }
        $prot = $prot.$cod;
      }
      for($i = 50; $i < strlen($prot); $i = $i+51)
        $prot = substr_replace($prot,"\n",$i,0);
      $response["prot"] = $prot;
    }

    for($i = 50; $i < strlen($dna); $i = $i+51)
      $dna = substr_replace($dna,"\n",$i,0);
    $response["dna"] = $dna;
  } else {
    $response = false;
  }
  echo json_encode($response);
}

function location_decode($r){
  $aux = explode(":",$r);
  $chr = $aux[0];
  $start = explode("-",$aux[1])[0];
  $end = explode("-",$aux[1])[1];
  return array($chr,$start,$end);
}
?>
