wwwDirectory = function(){
  path <- system.file("www",package="D3GB")
  return(path)
}

createHTML <- function(dir, dependencies, data, chromosomes){
  indexfile <- file.path(dir, "index.html")
  if(file.exists(dir)){
    if(file.exists(indexfile)){
      content <- scan(file = indexfile, what = character(0), sep = "\n", quiet = TRUE)
      if(sum(content=="<!--BioinfoUSAL/D3GB-->")==1){
        unlink(dir, recursive = TRUE)
      }else{
        stop(paste0("dir: '",dir,"' already exists"))
      }
    }else{
      stop(paste0("dir: '",dir,"' already exists"))
    }
  }
  dir.create(dir)
  www <- wwwDirectory()
  html <- scan(file = file.path(www, "template.html"), what = character(0), sep = "\n", quiet = TRUE)
  name <- strsplit(dir,"/")[[1]]
  name <- name[length(name)]
  html <- sub("<!--title-->", name, html)
  dep <- "<!--head-->"
  for(i in seq_along(dependencies)){
    if(grepl(".css$",dependencies[i])){
      dep <- paste(dep, paste0("<link rel=\"stylesheet\" type=\"text/css\" href=\"styles/",dependencies[i],"\"></link>"), sep = "\n")
      dirName <- "styles"
    }else{
      dep <- paste(dep, paste0("<script type=\"text/javascript\" src=\"scripts/",dependencies[i],"\"></script>"), sep = "\n")
      dirName <- "scripts"
    }
    dir.create(file.path(dir, dirName),FALSE)
    file.copy(file.path(www, dependencies[i]), file.path(dir, dirName))
  }
  html <- sub("<!--head-->", dep, html)
  chromosomes <- paste0('<script type="application/json" id="chromosomes">',chromosomes,'</script>')
  html <- sub("<!--body-->",paste("<!--body-->", chromosomes, sep = "\n"),html)
  data <- paste0('<script type="application/json" id="data">',data,'</script>')
  html <- sub("<!--body-->",paste("<!--body-->", data, sep = "\n"),html)
  write(html, indexfile)
  message(paste0("The graph has been generated in the ",dir," folder."))
}

base64encode <- function(filename) {
  to.read = file(filename, "rb")
  fsize <- file.size(filename)
  sbit <- readBin(to.read, raw(), n = fsize, endian = "little")
  close(to.read)
  b64c <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  shfts <- c(18,12,6,0)
  sand <- function(n,s) bitwAnd(bitwShiftR(n,s),63)+1
  slft <- function(p,n) bitwShiftL(as.integer(p),n)
  subs <- function(s,n) substring(s,n,n)
  npad <- ( 3 - length(sbit) %% 3) %% 3
  sbit <- c(sbit,as.raw(rep(0,npad)))
  pces <- lapply(seq(1,length(sbit),by=3),function(ii) sbit[ii:(ii+2)])
  encv <- paste0(sapply(pces,function(p) paste0(sapply(shfts,function(s)(subs(b64c,sand(slft(p[1],16)+slft(p[2],8)+slft(p[3],0),s)))))),collapse="")
  if (npad > 0) substr(encv,nchar(encv)-npad+1,nchar(encv)) <- paste0(rep("=",npad),collapse="")
  return(encv)
}

