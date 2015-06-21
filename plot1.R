# Function that builds 1st  plot. Assumes that package 'plyr' is loaded into workspace
# Parameters :
#             dir.working      [optional] - folder that will contain generated image
#             output.file.name [optional] - name of the output file
#             image.width      [optional] - width of the image to be generated
#             image.height     [optional] - height of the image to be generated
plot1<-function(dir.working = "./", output.file.name="plot1.png", image.width=800, image.height=600){
    
    subfolder = "./dataset"
    
    archive.file.name = sprintf("%s/archive.zip", subfolder);
    dataset.file.name.1 = sprintf("%s/summarySCC_PM25.rds", subfolder);
    dataset.file.name.2 = sprintf("%s/Source_Classification_Code.rds", subfolder);
    # save current and set working directory     
    dir.current<-getwd();
    setwd(dir.working);
    
    # the very first step - make sure that dataset file exists 
    if(!file.exists(dataset.file.name.1) | !file.exists(dataset.file.name.2) ){
        
        # file does not exists. download one        
        # -- dataset will be put into subdirectory. create one if necessary 
        if(!file.exists(subfolder)){
            dir.create(subfolder);
        }
        
        # --  download archive file that contains dataset
        download.file(url="https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", dest=archive.file.name, method="curl", quiet=T);
        if(!file.exists(archive.file.name)){
            stop("unable to download dataset archive file.");
        }
        print(getwd());
        
        # -- extract file from archive and verify that extraction succeeded
        unzip(archive.file.name, junkpaths = T, exdir=subfolder);
        if(!file.exists(dataset.file.name.1) | !file.exists(dataset.file.name.2)){
            stop("unable to unzip dataset archive file");
        }
        
        # -- no need for archive file any more
        unlink(archive.file.name);
    }

    # second step - load datasets internally
    #dsNEI <- readRDS(dataset.file.name.1)
    #dsSCC <- readRDS(dataset.file.name.2)
    
    dsw<-ddply(readRDS(dataset.file.name.1), .(year), summarize, mean=mean(Emissions))
    model<-lm(mean~year, dsw);

    # generate image
    # -- create context
    png(output.file.name, width=image.width, height=image.height, bg="white");

    # -- create 
    plot(dsw$year, dsw$mean, type="p", pch=19, xlab="year", ylab="Average amount of PM2.5 emitted, tons", col="dark red", main="PM2.5 emission change in the United States");
    lines(dsw$year, dsw$mean, type="h", col="grey", lty=3);
    abline(model, col="blue", lty=3);
    
    # finish
    dev.off();
    
    # restore original working directory
    setwd(dir.current);
    
    # build return value
    paste(dir.working, output.file.name, sep="");
    normalizePath(paste(dir.working, output.file.name, sep=""));
}
    