# Function that builds 3rd plot. Assumes that packages 'plyr' and 'ggplot2' are loaded into workspace
# Parameters :
#             dir.working      [optional] - folder that will contain generated image
#             output.file.name [optional] - name of the output file
#             image.width      [optional] - width of the image to be generated
#             image.height     [optional] - height of the image to be generated
plot3<-function(dir.working = "./", output.file.name="plot3.png", image.width=800, image.height=600){
    
    subfolder = "./dataset";
    resolution = 150;
    
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

    # second step - prepare dataset
    # -- load dataset
    dsNEI <- readRDS(dataset.file.name.1);
    # -- filter dataset
    dsNEI <- dsNEI[dsNEI$fips == "24510", ];

    # generate image
    # -- draw it
    pl<- ggplot(dsNEI, aes(x=year, y=Emissions, group = type)) + 
                stat_summary(fun.y="mean", geom = c("point")) + 
                facet_grid(type~., scales="free", ) + 
                geom_smooth(method="loess") + 
                theme(plot.title = element_text(size=9, face="bold", vjust=2), axis.title=element_text(size=8), strip.text.y = element_text(size=5)) +
                ggtitle("Changes in emission in Baltimore city by source type") + 
                ylab(expression("Average amount of " * PM[2.5] * " emitted, tons"));
    
    # -- save ti
    ggsave(filename = output.file.name, plot = pl, width = image.width/resolution, height=image.height/resolution, dpi=resolution);

    # restore original working directory
    setwd(dir.current);
    
    # build return value
    paste(dir.working, output.file.name, sep="");
    normalizePath(paste(dir.working, output.file.name, sep=""));
}
    