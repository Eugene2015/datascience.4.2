# Function that builds 6th plot. Assumes that package 'ggplot2' is loaded into workspace
# Parameters :
#             dir.working      [optional] - folder that will contain generated image
#             output.file.name [optional] - name of the output file
#             image.width      [optional] - width of the image to be generated
#             image.height     [optional] - height of the image to be generated
plot6<-function(dir.working = "./", output.file.name="plot6.png", image.width=800, image.height=600){
    
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
    dsSCC <- readRDS(dataset.file.name.2);

    # -- filter datasets

    # -- -- Baltimore and LA only
    dsNEI <- dsNEI[dsNEI$fips == "24510" | dsNEI$fips == "06037", ];
    # -- vehicles (all kind)
    dsSCC<-dsSCC[grepl("vehicle", dsSCC$EI.Sector, ignore.case=T), ];
    filter<-dsNEI$SCC %in% dsSCC$SCC
    dsNEI <- dsNEI[filter,];

    # -- -- generate dataframe that is to be plotted.  
    # -- -- year | fips | mean value - drop unnecessary columns
    dsNEI$SCC<-NULL; dsNEI$Pollutant<-NULL; dsNEI$type<-NULL;
    # 
    # -- -- calculate pollution values by year and by region
    dsNEI<-aggregate(dsNEI, list(Region=dsNEI$fips, Year=dsNEI$year), mean);

    dsRegions<-c("Baltimore", "Los Angeles");
    dsChanges<-c((dsNEI[dsNEI$Region == "24510" & dsNEI$Year==2008, ]$Emissions - dsNEI[dsNEI$Region == "24510" & dsNEI$Year==1999, ]$Emissions), 
                 (dsNEI[dsNEI$Region == "06037" & dsNEI$Year==2008, ]$Emissions - dsNEI[dsNEI$Region == "06037" & dsNEI$Year==1999, ]$Emissions));
    
    dsDifference<-data.frame(Region=dsRegions, Changes=dsChanges);
    
    pl1<-ggplot(ds, aes(x = Region, y = Changes, fill = Region)) + 
         geom_bar(stat = "identity", alpha=3/4) +
         ggtitle("Difference in emission from vehicle-related sources in Baltimore and Los Angeles\nbetween 1999 and 2008") + 
         theme(plot.title = element_text(size=8, face="bold", vjust=2, hjust=0.2), axis.title=element_text(size=7)) +
         coord_flip() + 
         ylab(expression("Difference in amount of " * PM[2.5] * " emitted, tons"));
    
    # -- stacked chart
    #pl2 <-ggplot(dsNEI, aes(x = Year, y = Emissions, fill = Region)) + 
    #     geom_bar(stat = "identity", alpha=3/4) +
    #     ggtitle("Changes in emission from vehicle-related sources in Baltimore and Los Angeles") + 
    #     theme(plot.title = element_text(size=9, face="bold", vjust=2, hjust=0.2), axis.title=element_text(size=8)) +
    #     ylab(expression("Average amount of " * PM[2.5] * " emitted, tons"));
    

    # pl<-grid.arrange(pl1, pl2,nrow=1, ncol=2, clip = T, plot=F);
    
    #pl<- ggplot(dsNEI, aes(x=year, y=Emissions)) + 
    #     stat_summary(fun.y="mean", geom = "bar", fill=dsNEI$fips,  color="black") +  
    #     ggtitle("Changes in emission from vehicle-related sources in Baltimore City") + 
    #     theme(plot.title = element_text(size=10, face="bold", vjust=2, hjust = 2), axis.title=element_text(size=9)) +
    #     ylab(expression("Average amount of " * PM[2.5] * " emitted, tons"));
    
    
    # -- save ti
    ggsave(filename = output.file.name, width = image.width/resolution, height=image.height/resolution, dpi=resolution);

    # restore original working directory
    setwd(dir.current);
    
    # build return value
    paste(dir.working, output.file.name, sep="");
    normalizePath(paste(dir.working, output.file.name, sep=""));
}
    