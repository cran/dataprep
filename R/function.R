descdata = function(data, start = NULL, end = NULL, stats= 1:9, first = 'variables') {
  t0=Sys.time()
  if(length(stats)==1) {
    if(is.numeric(stats)) {
      result = data.frame(sapply(data[,start:end],
                                 function(x) eval(parse(text=c('sum(!is.na(x))', 'sum(is.na(x))', 'mean(x, na.rm = TRUE)', 'sd(x, na.rm = TRUE)',
                                                               'median(x, na.rm = TRUE)', 'mean(x, trim = .1, na.rm = TRUE)', 'min(x, na.rm = TRUE)',
                                                               'max(x, na.rm = TRUE)', 'IQR(x, na.rm = TRUE)')[stats]))))
      names(result) = c('n','na','mean','sd','median','trimmed','min','max','IQR')[stats]
    } else {
      result = data.frame(sapply(data[,start:end],
                                 function(x) eval(parse(text=c('sum(!is.na(x))', 'sum(is.na(x))', 'mean(x, na.rm = TRUE)', 'sd(x, na.rm = TRUE)',
                                                               'median(x, na.rm = TRUE)', 'mean(x, trim = .1, na.rm = TRUE)', 'min(x, na.rm = TRUE)',
                                                               'max(x, na.rm = TRUE)', 'IQR(x, na.rm = TRUE)')[which(c('n','na','mean','sd','median','trimmed','min','max','IQR')==stats)]))))
      names(result) = stats
    }
  } else {
    result = data.frame(t(sapply(data[,start:end],
                                 function(x) c(n = sum(!is.na(x)), na = sum(is.na(x)), mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE),
                                               median = median(x, na.rm = TRUE), trimmed= mean(x, trim = .1, na.rm = TRUE), min = min(x, na.rm = TRUE),
                                               max = max(x, na.rm = TRUE), IQR = IQR(x, na.rm = TRUE))[stats])))
  }
  if (all(!grepl("\\D",gsub('[.]','',names(data[start:end]))))) {
    result = data.frame(first=as.numeric(rownames(result)), result, row.names = NULL)
  } else {
    result=data.frame(first=rownames(result), result, row.names = NULL)
  }
  names(result)[1] = first
  cat(paste0('Time used by descdata: ',format(Sys.time()-t0,digits=3)),'\n')
  result
}

melt=function(data,cols=NULL) {
  t0=Sys.time()
  a=data
  b=cbind(a[rep(1:nrow(a),ncol(a[-c(cols)])),cols],variable=factor(rep(names(a[-c(cols)]),each=nrow(a)),levels=names(a[-c(cols)])),value=unlist(a[-c(cols)]))
  rownames(b)=NULL
  cat(paste0('Time used by melt: ',format(Sys.time()-t0,digits=3)),'\n')
  b
}

descplot=function(data, start = NULL, end = NULL, stats= 1:9, first = 'variables') {
  t0=Sys.time()
  if (all(!grepl("\\D",gsub('[.]','',names(data[start:end]))))) {
    if (length(find.package('reshape2',quiet=TRUE))==1) {
      df=reshape2::melt(descdata(data, start = start, end = end, stats=stats, first = first),1)
      plot=ggplot2::ggplot(df,ggplot2::aes(variables,value))+
        ggplot2::geom_line()+
        ggplot2::scale_x_log10()+
        ggplot2::facet_wrap(.~variable,ncol=ifelse(length(stats)==4,2,3),scales='free_y')
    } else {
      df=melt(descdata(data, start = start, end = end, stats=stats, first = first),1)
      df$variables=factor(df$variables,levels = unique(names(data[start:end])))
      plot=ggplot2::ggplot(df,ggplot2::aes(variables,value))+
        ggplot2::geom_line()+
        ggplot2::scale_x_log10()+
        ggplot2::facet_wrap(.~variable,ncol=ifelse(length(stats)==4,2,3),scales='free_y')
    }
  } else{
    if (length(find.package('reshape2',quiet=TRUE))==1) {
      df=reshape2::melt(descdata(data, start = start, end = end, stats=stats, first = first),1)
      df$variables=factor(df$variables,levels = unique(names(data[start:end])))
      plot=ggplot2::ggplot(df,ggplot2::aes(variables,value))+
        ggplot2::geom_col()+
        ggplot2::facet_wrap(.~variable,ncol=ifelse(length(stats)==4,2,3),scales='free_y')
    } else {
      df=melt(descdata(data, start = start, end = end, stats=stats, first = first),1)
      df$variables=factor(df$variables,levels = unique(names(data[start:end])))
      plot=ggplot2::ggplot(df,ggplot2::aes(variables,value))+
        ggplot2::geom_col()+
        ggplot2::facet_wrap(.~variable,ncol=ifelse(length(stats)==4,2,3),scales='free_y')
    }
  }
  cat(paste0('Time used by descplot: ',format(Sys.time()-t0,digits=3)),'\n')
  plot
}

varidele=function(data,start=NULL,end=NULL,fraction=.25){
  t0=Sys.time()
  a=data[,start:end][rowSums(is.na(data[,start:end]))!=ncol(data[,start:end]),]
  b=c(colSums(is.na(a))/nrow(a))
  c=which(colnames(data)%in%names(b[b>=fraction]))
  d=setdiff(start:end,c)
  if(end<ncol(data)) {
    result=data[,c(1:(start-1),min(d):max(d),(end+1):ncol(data))]
  } else {
    result=data[,c(1:(start-1),min(d):max(d))]
  }
  cat(paste0(ncol(data)-ncol(result),' variables are deleted: ',paste(setdiff(names(data),names(result)),collapse=', ')),'\n')
  cat(paste0('Time used by varidele: ',format(Sys.time()-t0,digits=3)),'\n')
  result
}

percdata=function(data,start=NULL,end=NULL,group=NULL,diff=.1,part='both') {
  t0=Sys.time()
  if(part%in%c('both',2)) {
    seq=c(seq(0,diff*5,diff),seq(100-diff*5,100,diff))
  }
  if(part%in%c('bottom',0)) {
    seq=seq(0,diff*5,diff)
  }
  if(part%in%c('top',1)) {
    seq=seq(100-diff*5,100,diff)
  }
  if(is.null(group)||!is.numeric(group)||group<1) {
    a=apply(data[,start:end], 2, function(x) quantile(x, seq/100, na.rm = TRUE))
    result=cbind.data.frame(a,percentile=paste0(seq,'th'))
  } else {
    a=do.call(cbind, aggregate(data[,start:end], list(data[,group]), function(x) quantile(x, seq/100, na.rm = TRUE)))
    b=ncol(a)
    if(part%in%c('both',2)) {i=1:12} else {i=1:6}
    if(part%in%c('both',2)) {
      result=cbind.data.frame(unlist(eval(parse(text=paste0(c('rbind(',paste0('a[,c(1,seq(1+',i,',',b,'-12+',i,',12))]',collapse = ','),')'),collapse = '')))),
                              percentile=rep(paste0(seq,'th'),each=length(unique(data[,group]))))
    } else {
      result=cbind.data.frame(unlist(eval(parse(text=paste0(c('rbind(',paste0('a[,c(1,seq(1+',i,',',b,'-6+',i,',6))]',collapse = ','),')'),collapse = '')))),
                              percentile=rep(paste0(seq,'th'),each=length(unique(data[,group]))))
    }
    colnames(result) = c(colnames(data[,c(group,start:end)]), 'percentile')
    result[,1]=factor(result[,1],labels=unique(data[,group]))
    result[,2:(ncol(result)-1)]=lapply(result[,2:(ncol(result)-1)], as.numeric)
  }
  result$percentile=factor(result$percentile,levels=unique(result$percentile))
  cat(paste0('Time used by percdata: ',format(Sys.time()-t0,digits=3)),'\n')
  result
}

zerona=function(x) {
  x[x==0]=NA
  x
}

percplot=function(data,start=NULL,end=NULL,group=NULL,ncol=NULL,diff=.1,part='both') {
  t0=Sys.time()
  df=percdata(data,start,end,group,diff,part)
  if(is.null(group)||!is.numeric(group)||group<1) {
    if(all(!grepl("\\D",gsub('[.]','',names(df[-c(1,ncol(df))]))))) {
      if(length(find.package('reshape2',quiet=TRUE))==1) {
        if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))<(end-start+1)/10&
           max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)>=10^3) {
          plot=ggplot2::ggplot(reshape2::melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
            ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
            ggplot2::scale_x_log10(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
            ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))
          if(part%in%c('both',2)) {
            plot=plot+ggplot2::scale_y_log10(labels=scales::trans_format("log10",scales::math_format(10^.x)),sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
          } else {
            plot=plot+ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
          }
        } else {
          if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))<(end-start+1)/10&
             max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)<10^3) {
            plot=ggplot2::ggplot(reshape2::melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
              ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
              ggplot2::scale_x_log10(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
              ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
              ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))
          } else {
            if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))>=(end-start+1)/10&
               max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)>=10^3) {
              plot=ggplot2::ggplot(reshape2::melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
                ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
                ggplot2::scale_x_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))
              if(part%in%c('both',2)) {
                plot=plot+ggplot2::scale_y_log10(labels=scales::trans_format("log10",scales::math_format(10^.x)),sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
              } else {
                plot=plot+ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
              }
            } else {
              plot=ggplot2::ggplot(reshape2::melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
                ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
                ggplot2::scale_x_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))
            }
          }
        }
      } else {
        if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))<(end-start+1)/10&
           max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)>=10^3) {
          plot=ggplot2::ggplot(melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
            ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
            ggplot2::scale_x_log10(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
            ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))
          if(part%in%c('both',2)) {
            plot=plot+ggplot2::scale_y_log10(labels=scales::trans_format("log10",scales::math_format(10^.x)),sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
          } else {
            plot=plot+ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
          }
        } else {
          if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))<(end-start+1)/10&
             max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)<10^3) {
            plot=ggplot2::ggplot(melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
              ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
              ggplot2::scale_x_log10(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
              ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
              ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))
          } else {
            if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))>=(end-start+1)/10&
               max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)>=10^3) {
              plot=ggplot2::ggplot(melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
                ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
                ggplot2::scale_x_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))
              if(part%in%c('both',2)) {
                plot=plot+ggplot2::scale_y_log10(labels=scales::trans_format("log10",scales::math_format(10^.x)),sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
              } else {
                plot=plot+ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
              }
            } else {
              plot=ggplot2::ggplot(melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
                ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
                ggplot2::scale_x_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))
            }
          }
        }
      }
    } else {
      if(part%in%c('both',2)) {
        df=cbind(df,part=c(rep('bottom',nrow(df)/2),rep('top',nrow(df)/2)))
        df$part=factor(df$part,levels=c('top','bottom'))
      }
      if(part%in%c('bottom',0)) {
        df=cbind(df,part=rep('bottom',nrow(df)))
      }
      if(part%in%c('top',1)) {
        df=cbind(df,part=rep('top',nrow(df)))
      }
      if (length(find.package('reshape2',quiet=TRUE))==1) {
        plot=ggplot2::ggplot(reshape2::melt(df,c(ncol(df)-1,ncol(df))),ggplot2::aes(variable,value))
      } else {
        plot=ggplot2::ggplot(melt(df,c(ncol(df)-1,ncol(df))),ggplot2::aes(variable,value))
      }
      plot=plot+ggplot2::geom_col(ggplot2::aes(fill=percentile),position=ggplot2::position_stack(reverse=TRUE))+
        ggplot2::scale_fill_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
        ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
        ggplot2::guides(fill=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))
      if(part%in%c('both',2)) {
        plot=plot+ggplot2::facet_grid(part~.,scales='free_y')
      }
    }
  } else {
    if (all(!grepl("\\D",gsub('[.]','',names(df[-c(1,ncol(df))]))))) {
      if (length(find.package('reshape2',quiet=TRUE))==1) {
        if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))<(end-start+1)/10&
           max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)>=10^3) {
          plot=ggplot2::ggplot(reshape2::melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
            ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
            ggplot2::scale_x_log10(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
            ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
            ggplot2::facet_wrap(facets=names(data[group]),ncol=ncol)+
            ggplot2::geom_text(ggplot2::aes(median(as.numeric(names(data[,start:end])),na.rm=TRUE),10^(quantile(log10(range(zerona(data[,start:end]),na.rm=TRUE)),.1)),
                                            label=paste0("n=",n)),setNames(aggregate(1:nrow(data),list(data[,group]),length),c(names(data[group]),'n')),inherit.aes=FALSE)
          if(part%in%c('both',2)) {
            plot=plot+ggplot2::scale_y_log10(labels=scales::trans_format("log10",scales::math_format(10^.x)),sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
          } else {
            plot=plot+ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
          }
        } else {
          if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))<(end-start+1)/10&
             max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)<10^3) {
            plot=ggplot2::ggplot(reshape2::melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
              ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
              ggplot2::scale_x_log10(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
              ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
              ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
              ggplot2::facet_wrap(facets=names(data[group]),ncol=ncol)+
              ggplot2::geom_text(ggplot2::aes(median(as.numeric(names(data[,start:end])),na.rm=TRUE),10^(quantile(log10(range(zerona(data[,start:end]),na.rm=TRUE)),.1)),
                                              label=paste0("n=",n)),setNames(aggregate(1:nrow(data),list(data[,group]),length),c(names(data[group]),'n')),inherit.aes=FALSE)
          } else {
            if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))>=(end-start+1)/10&
               max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)>=10^3) {
              plot=ggplot2::ggplot(reshape2::melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
                ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
                ggplot2::scale_x_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
                ggplot2::facet_wrap(facets=names(data[group]),ncol=ncol)+
                ggplot2::geom_text(ggplot2::aes(median(as.numeric(names(data[,start:end])),na.rm=TRUE),10^(quantile(log10(range(zerona(data[,start:end]),na.rm=TRUE)),.1)),
                                                label=paste0("n=",n)),setNames(aggregate(1:nrow(data),list(data[,group]),length),c(names(data[group]),'n')),inherit.aes=FALSE)
              if(part%in%c('both',2)) {
                plot=plot+ggplot2::scale_y_log10(labels=scales::trans_format("log10",scales::math_format(10^.x)),sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
              } else {
                plot=plot+ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
              }
            } else {
              plot=ggplot2::ggplot(reshape2::melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
                ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
                ggplot2::scale_x_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
                ggplot2::facet_wrap(facets=names(data[group]),ncol=ncol)+
                ggplot2::geom_text(ggplot2::aes(median(as.numeric(names(data[,start:end])),na.rm=TRUE),10^(quantile(log10(range(zerona(data[,start:end]),na.rm=TRUE)),.1)),
                                                label=paste0("n=",n)),setNames(aggregate(1:nrow(data),list(data[,group]),length),c(names(data[group]),'n')),inherit.aes=FALSE)
            }
          }
        }
      } else {
        if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))<(end-start+1)/10&
           max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)>=10^3) {
          plot=ggplot2::ggplot(melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
            ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
            ggplot2::scale_x_log10(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
            ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
            ggplot2::facet_wrap(facets=names(data[group]),ncol=ncol)+
            ggplot2::geom_text(ggplot2::aes(median(as.numeric(names(data[,start:end])),na.rm=TRUE),10^(quantile(log10(range(zerona(data[,start:end]),na.rm=TRUE)),.1)),
                                            label=paste0("n=",n)),setNames(aggregate(1:nrow(data),list(data[,group]),length),c(names(data[group]),'n')),inherit.aes=FALSE)
          if(part%in%c('both',2)) {
            plot=plot+ggplot2::scale_y_log10(labels=scales::trans_format("log10",scales::math_format(10^.x)),sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
          } else {
            plot=plot+ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
          }
        } else {
          if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))<(end-start+1)/10&
             max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)<10^3) {
            plot=ggplot2::ggplot(melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
              ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
              ggplot2::scale_x_log10(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
              ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
              ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
              ggplot2::facet_wrap(facets=names(data[group]),ncol=ncol)+
              ggplot2::geom_text(ggplot2::aes(median(as.numeric(names(data[,start:end])),na.rm=TRUE),10^(quantile(log10(range(zerona(data[,start:end]),na.rm=TRUE)),.1)),
                                              label=paste0("n=",n)),setNames(aggregate(1:nrow(data),list(data[,group]),length),c(names(data[group]),'n')),inherit.aes=FALSE)
          } else {
            if(length(unique(signif(diff(log(as.numeric(as.character(names(data[,start:end]))))),2)))>=(end-start+1)/10&
               max(data[,start:end],na.rm=TRUE)/min(data[,start:end],na.rm=TRUE)>=10^3) {
              plot=ggplot2::ggplot(melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
                ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
                ggplot2::scale_x_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
                ggplot2::facet_wrap(facets=names(data[group]),ncol=ncol)+
                ggplot2::geom_text(ggplot2::aes(median(as.numeric(names(data[,start:end])),na.rm=TRUE),10^(quantile(log10(range(zerona(data[,start:end]),na.rm=TRUE)),.1)),
                                                label=paste0("n=",n)),setNames(aggregate(1:nrow(data),list(data[,group]),length),c(names(data[group]),'n')),inherit.aes=FALSE)
              if(part%in%c('both',2)) {
                plot=plot+ggplot2::scale_y_log10(labels=scales::trans_format("log10",scales::math_format(10^.x)),sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
              } else {
                plot=plot+ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))
              }
            } else {
              plot=ggplot2::ggplot(melt(df,c(1,ncol(df))),ggplot2::aes(as.numeric(as.character(variable)),value,color=percentile))+ggplot2::geom_line()+
                ggplot2::scale_color_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
                ggplot2::scale_x_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
                ggplot2::guides(colour=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
                ggplot2::facet_wrap(facets=names(data[group]),ncol=ncol)+
                ggplot2::geom_text(ggplot2::aes(median(as.numeric(names(data[,start:end])),na.rm=TRUE),10^(quantile(log10(range(zerona(data[,start:end]),na.rm=TRUE)),.1)),
                                                label=paste0("n=",n)),setNames(aggregate(1:nrow(data),list(data[,group]),length),c(names(data[group]),'n')),inherit.aes=FALSE)
            }
          }
        }
      }
    } else {
      names(df)[1]='groups'
      if(part%in%c('both',2)) {
        df=cbind(df,part=c(rep('bottom',nrow(df)/2),rep('top',nrow(df)/2)))
        df$part=factor(df$part,levels=c('top','bottom'))
      }
      if(part%in%c('bottom',0)) {
        df=cbind(df,part=rep('bottom',nrow(df)))
      }
      if(part%in%c('top',1)) {
        df=cbind(df,part=rep('top',nrow(df)))
      }
      if (length(find.package('reshape2',quiet=TRUE))==1) {
        plot=ggplot2::ggplot(reshape2::melt(df,c(1,ncol(df)-1,ncol(df))),ggplot2::aes(variable,value))
      } else {
        plot=ggplot2::ggplot(melt(df,c(1,ncol(df)-1,ncol(df))),ggplot2::aes(variable,value))
      }
      if(part%in%c('both',2)) {
        plot=plot+ggplot2::geom_col(ggplot2::aes(fill=percentile),position=ggplot2::position_stack(reverse=TRUE))+
          ggplot2::scale_fill_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
          ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
          ggplot2::guides(fill=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
          ggplot2::geom_text(ggplot2::aes((ncol(df)-3)/2,y,label=paste0("n=",n)),
                             transform(merge(setNames(aggregate(df[names(which.max(colMeans(df[sapply(df,is.numeric)],na.rm=TRUE)))],list(df[,1],df[,ncol(df)]),sum),c('groups','part','y')),
                                             setNames(aggregate(1:nrow(data),list(data[,group]),length),c('groups','n')),'groups'),y=y*1.05),inherit.aes=FALSE)+
          ggplot2::facet_grid(part~groups,scales='free_y')
      } else {
        plot=plot+ggplot2::geom_col(ggplot2::aes(fill=percentile),position=ggplot2::position_stack(reverse=TRUE))+
          ggplot2::scale_fill_manual(values=c("grey","red","green","pink","gold","forestgreen","orange","blue","purple","cyan","brown","black"))+
          ggplot2::scale_y_continuous(sec.axis=ggplot2::dup_axis(name=NULL,labels=NULL))+
          ggplot2::guides(fill=ggplot2::guide_legend(title=paste0("n:",nrow(data),"\nna:",sum(is.na(data)),"\n\nPercentiles"),reverse=TRUE,override.aes=list(size=1)))+
          ggplot2::geom_text(ggplot2::aes((ncol(df)-3)/2,y,label=paste0("n=",n)),
                             transform(merge(setNames(aggregate(df[names(which.max(colMeans(df[sapply(df,is.numeric)],na.rm=TRUE)))],list(df[,1],df[,ncol(df)]),sum),c('groups','part','y')),
                                             setNames(aggregate(1:nrow(data),list(data[,group]),length),c('groups','n')),'groups'),y=y*1.05),inherit.aes=FALSE)+
          ggplot2::facet_wrap(facets='groups',ncol=ncol)
      }
    }
  }
  cat(paste0('Time used by percplot: ',format(Sys.time()-t0,digits=3)),'\n')
  plot
}

obsedele=function(data,start=NULL,end=NULL,group=NULL,by="min",half=30,cores=NULL) {
  t0=Sys.time()
  df=data
  if(is.null(group)||!is.numeric(group)||group<1) {
    unit=c("secs","mins","hours","days","weeks")
    num=ifelse(grepl("^[A-Za-z]+$",by),1,as.numeric(gsub(".*?([0-9]+).*","\\1",by)))
    char=gsub(".*?([a-z]+).*", "\\1",by)
    data=transform(data,period=cumsum(ifelse(c(0,as.numeric(diff(date),units=unit[grepl(char,unit)]))>half*num|c(0,as.numeric(diff(date),units=unit[grepl(char,unit)]))==0,1,0)),
                   Date=date,check.names=FALSE)
    if(length(find.package(c('foreach','doParallel','parallel'),quiet=TRUE))==3&as.numeric(object.size(data))/1024/1024>10) {
      cores=ifelse(is.null(cores),parallel::detectCores(),cores)
      cl=parallel::makeCluster(cores)
      doParallel::registerDoParallel(cl)
      c=foreach::foreach(i=unique(data$period),.combine=rbind) %dopar% {
        a=data[data$period==i,]
        b=merge(a,data.frame(date=seq(min(a$date),max(a$date),by=by)),all.y=TRUE)
        if(any(is.na(b[1,start:end]))) {
          cond=sapply(b[start:end],function(x) ifelse(all(is.na(x)),Inf,min(which(!is.na(x)))))
          if(any(cond>half)&!any(is.infinite(cond))) {
            b=b[(max(cond)-half):nrow(b),]
          }
          if(any(is.infinite(cond))) {
            b=NULL
          }
        }
        if(any(is.na(b[nrow(b),start:end]))) {
          cond=sapply(b[nrow(b):1,][start:end],function(x) ifelse(all(is.na(x)),Inf,min(which(!is.na(x)))))
          if(any(cond>half)&!any(is.infinite(cond))) {
            b=b[1:(nrow(b)-max(cond)+half),]
          }
          if(any(is.infinite(cond))) {
            b=NULL
          }
        }
        if(!is.null(b)) {
          vec=b[,start:end][b[,start:end]<Inf]
          id=data.table::rleid(vec)
          runid=data.table::rowid(id)
          DT=data.table::data.table(runid=runid,by=id)
          vec[DT$runid<=half]=0
          runidrev=data.table::setorder(DT,by,-runid)$runid
          vec[runidrev<=half]=0
          b[,start:end]=vec
          b
        }
      }
      parallel::stopCluster(cl)
    } else {
      dflist=list()
      for (i in unique(data$period)) {
        a=data[data$period==i,]
        b=merge(a,data.frame(date=seq(min(a$date),max(a$date),by=by)),all.y=TRUE)
        if(any(is.na(b[1,start:end]))) {
          cond=sapply(b[start:end],function(x) ifelse(all(is.na(x)),Inf,min(which(!is.na(x)))))
          if(any(cond>half)&!any(is.infinite(cond))) {
            b=b[(max(cond)-half):nrow(b),]
          }
          if(any(is.infinite(cond))) {
            b=NULL
          }
        }
        if(any(is.na(b[nrow(b),start:end]))) {
          cond=sapply(b[nrow(b):1,][start:end],function(x) ifelse(all(is.na(x)),Inf,min(which(!is.na(x)))))
          if(any(cond>half)&!any(is.infinite(cond))) {
            b=b[1:(nrow(b)-max(cond)+half),]
          }
          if(any(is.infinite(cond))) {
            b=NULL
          }
        }
        if(!is.null(b)) {
          vec=b[,start:end][b[,start:end]<Inf]
          id=data.table::rleid(vec)
          runid=data.table::rowid(id)
          DT=data.table::data.table(runid=runid,by=id)
          vec[DT$runid<=half]=0
          runidrev=data.table::setorder(DT,by,-runid)$runid
          vec[runidrev<=half]=0
          b[,start:end]=vec
          b$i=i
          dflist[[i]]=b
        }
      }
      c=do.call(rbind,dflist)
    }
  } else {
    if(group>=start&group<=end) {
      stop('Please move the column group to before start or after end')
    } else {
      data=transform(data,Date=date,check.names=FALSE)
      data[,group]=factor(data[,group],labels=1:length(unique(data[,group])))
      if(length(find.package(c('foreach','doParallel','parallel'),quiet=TRUE))==3&as.numeric(object.size(data))/1024/1024>10) {
        cores=ifelse(is.null(cores),parallel::detectCores(),cores)
        cl=parallel::makeCluster(cores)
        doParallel::registerDoParallel(cl)
        c=foreach::foreach(i=1:length(unique(data[,group])),.combine=rbind) %dopar% {
          a=data[data[,group]==i,]
          b=merge(a,data.frame(date=seq(min(a$date),max(a$date),by=by)),all.y=TRUE)
          if(any(is.na(b[1,start:end]))) {
            cond=sapply(b[start:end],function(x) ifelse(all(is.na(x)),Inf,min(which(!is.na(x)))))
            if(any(cond>half)&!any(is.infinite(cond))) {
              b=b[(max(cond)-half):nrow(b),]
            }
            if(any(is.infinite(cond))) {
              b=NULL
            }
          }
          if(any(is.na(b[nrow(b),start:end]))) {
            cond=sapply(b[nrow(b):1,][start:end],function(x) ifelse(all(is.na(x)),Inf,min(which(!is.na(x)))))
            if(any(cond>half)&!any(is.infinite(cond))) {
              b=b[1:(nrow(b)-max(cond)+half),]
            }
            if(any(is.infinite(cond))) {
              b=NULL
            }
          }
          if(!is.null(b)) {
            vec=b[,start:end][b[,start:end]<Inf]
            id=data.table::rleid(vec)
            runid=data.table::rowid(id)
            DT=data.table::data.table(runid=runid,by=id)
            vec[DT$runid<=half]=0
            runidrev=data.table::setorder(DT,by,-runid)$runid
            vec[runidrev<=half]=0
            b[,start:end]=vec
            b
          }
        }
        parallel::stopCluster(cl)
      } else {
        dflist=list()
        for (i in 1:length(unique(data[,group]))) {
          a=data[data[,group]==i,]
          b=merge(a,data.frame(date=seq(min(a$date),max(a$date),by=by)),all.y=TRUE)
          if(any(is.na(b[1,start:end]))) {
            cond=sapply(b[start:end],function(x) ifelse(all(is.na(x)),Inf,min(which(!is.na(x)))))
            if(any(cond>half)&!any(is.infinite(cond))) {
              b=b[(max(cond)-half):nrow(b),]
            }
            if(any(is.infinite(cond))) {
              b=NULL
            }
          }
          if(any(is.na(b[nrow(b),start:end]))) {
            cond=sapply(b[nrow(b):1,][start:end],function(x) ifelse(all(is.na(x)),Inf,min(which(!is.na(x)))))
            if(any(cond>half)&!any(is.infinite(cond))) {
              b=b[1:(nrow(b)-max(cond)+half),]
            }
            if(any(is.infinite(cond))) {
              b=NULL
            }
          }
          if(!is.null(b)) {
            vec=b[,start:end][b[,start:end]<Inf]
            id=data.table::rleid(vec)
            runid=data.table::rowid(id)
            DT=data.table::data.table(runid=runid,by=id)
            vec[DT$runid<=half]=0
            runidrev=data.table::setorder(DT,by,-runid)$runid
            vec[runidrev<=half]=0
            b[,start:end]=vec
            b$i=i
            dflist[[i]]=b
          }
        }
        c=do.call(rbind,dflist)
      }
    }
  }
  d=subset(c,!is.na(Date))
  e=transform(d,full=ifelse(complete.cases(d[,start:end]),1,0),check.names=FALSE)
  f=subset(e,full==1)
  g=subset(df,date%in%f$date)
  rownames(g)=NULL
  cat(paste0(nrow(df)-nrow(g),' observations are deleted'),'\n')
  cat(paste0('Time used by obsedele: ',format(Sys.time()-t0,digits=3)),'\n')
  g
}

condextr=function(data,start=NULL,end=NULL,group=NULL,top=.995,top.error=.1,top.magnitude=.2,bottom=.0025,bottom.error=.2,bottom.magnitude=.4,interval=10,by='min',half=30,times=10,cores=NULL) {
  t0=Sys.time()
  a=data
  if(is.null(group)||!is.numeric(group)||group<1) {
    warning('Without a group column, the deleted values may congregate in a minority of periods')
    for (j in 1:times) {
      for (i in 1:interval) {
        a[,start:end]=sapply(a[,start:end],function(x) {topquan=quantile(x,top,na.rm=TRUE)
        bottquan=quantile(x,bottom,na.rm=TRUE)
        ifelse(x==max(x,na.rm=TRUE)&x>topquan*(1+top.error)+10^(floor(log10(topquan)))*top.magnitude|
                 x==min(x,na.rm=TRUE)&x<bottquan*(1-bottom.error)-10^(floor(log10(bottquan)))*bottom.magnitude,NA,x)})
      }
      a=obsedele(a,start=start,end=end,group=group,by=by,half=half,cores=cores)
    }
  } else {
    if(length(find.package('dplyr',quiet=TRUE))==1) {
      for (j in 1:times) {
        for (i in 1:interval) {
          a=dplyr::mutate_at(dplyr::group_by_at(a,group),start:end,function(x) {topquan=quantile(x,top,na.rm=TRUE)
          bottquan=quantile(x,bottom,na.rm=TRUE)
          ifelse(x==max(x,na.rm=TRUE)&x>topquan*(1+top.error)+10^(floor(log10(topquan)))*top.magnitude|
                   x==min(x,na.rm=TRUE)&x<bottquan*(1-bottom.error)-10^(floor(log10(bottquan)))*bottom.magnitude,NA,x)})
        }
        a=obsedele(a,start=start,end=end,group=group,by=by,half=half,cores=cores)
      }
    } else {
      a[,group]=factor(a[,group],labels=1:length(unique(a[,group])))
      for (j in 1:times) {
        for (i in 1:interval) {
          for (k in 1:length(unique(a[,group]))) {
            a[a[,group]==k,start:end]=sapply(a[a[,group]==k,start:end],function(x) {topquan=quantile(x,top,na.rm=TRUE)
            bottquan=quantile(x,bottom,na.rm=TRUE)
            ifelse(x==max(x,na.rm=TRUE)&x>topquan*(1+top.error)+10^(floor(log10(topquan)))*top.magnitude|
                     x==min(x,na.rm=TRUE)&x<bottquan*(1-bottom.error)-10^(floor(log10(bottquan)))*bottom.magnitude,NA,x)})
          }
        }
        a=obsedele(a,start=start,end=end,group=group,by=by,half=half,cores=cores)
      }
      a[,group]=data[data$date%in%a$date,group]
    }
  }
  a=obsedele(a,start=start,end=end,group=group,by=by,half=half,cores=cores)
  rownames(a)=NULL
  cat(paste0(sum(is.na(a[start:end]))-sum(is.na(data[data$date%in%a$date,start:end])),' values are regarded as outliers and deleted excluding those in deleted observations'),'\n')
  cat(paste0(nrow(data)-nrow(a),' observations are deleted in total by condextr'),'\n')
  cat(paste0('Time used by condextr: ',format(Sys.time()-t0,digits=3)),'\n')
  a
}

percoutl=function(data,start=NULL,end=NULL,group=NULL,top=.995,bottom=.0025,by='min',half=30,cores=NULL) {
  t0=Sys.time()
  a=data
  if(is.null(group)||!is.numeric(group)||group<1) {
    a[,start:end]=sapply(a[,start:end],function(x) x=ifelse(x>quantile(x,top,na.rm=TRUE)|x<quantile(x,bottom,na.rm=TRUE),NA,x))
    a=obsedele(a,start=start,end=end,group=group,by=by,half=half,cores=cores)
  } else {
    if(length(find.package('dplyr',quiet=TRUE))==1) {
      a=dplyr::mutate_at(dplyr::group_by_at(a,group),start:end,function(x) x=ifelse(x>quantile(x,top,na.rm=TRUE)|x<quantile(x,bottom,na.rm=TRUE),NA,x))
      a=obsedele(a,start=start,end=end,group=group,by=by,half=half,cores=cores)
    } else {
      a[,group]=factor(a[,group],labels=1:length(unique(a[,group])))
      for (k in 1:length(unique(a[,group]))) {
        a[a[,group]==k,start:end]=sapply(a[a[,group]==k,start:end],function(x) x=ifelse(x>quantile(x,top,na.rm=TRUE)|x<quantile(x,bottom,na.rm=TRUE),NA,x))
      }
      a=obsedele(a,start=start,end=end,group=group,by=by,half=half,cores=cores)
      a[,group]=data[data$date%in%a$date,group]
    }
  }
  a=obsedele(a,start=start,end=end,group=group,by=by,half=half,cores=cores)
  cat(paste0(sum(is.na(a[start:end]))-sum(is.na(data[data$date%in%a$date,start:end])),' values are regarded as outliers and deleted excluding those in deleted observations'),'\n')
  cat(paste0(nrow(data)-nrow(a),' observations are deleted in total by percoutl'),'\n')
  cat(paste0('Time used by percoutl: ',format(Sys.time()-t0,digits=3)),'\n')
  a
}

optisolu=function(data,start=NULL,end=NULL,group=NULL,interval=35,times=10,top=.995,top.error=.1,top.magnitude=.2,bottom=.0025,bottom.error=.2,bottom.magnitude=.4,by='min',half=30,cores=NULL) {
  t0=Sys.time()
  if(length(find.package(c('foreach','doParallel','parallel'),quiet=TRUE))==3&as.numeric(object.size(data))/1024/1024>10) {
    cores=ifelse(is.null(cores),parallel::detectCores(),cores)
    cl=parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    c=foreach::foreach(i=1:(interval*times),.combine=rbind) %dopar% {
      inte= expand.grid(1:interval,1:times)$Var1[i]
      time=expand.grid(1:interval,1:times)$Var2[i]
      a=condextr(data,start=start,end=end,group=group,interval=inte,times=time,top=top,top.error=top.error,top.magnitude=top.magnitude,
                 bottom=bottom,bottom.error=bottom.error,bottom.magnitude=bottom.magnitude,by=by,half=half,cores=cores)
      b=cbind.data.frame(case=i,interval=inte,times=time,sdr=1-nrow(a)/nrow(data),orr=(sum(is.na(a[start:end]))-sum(is.na(data[data$date%in%a$date,start:end])))/(nrow(data)*ncol(data)),
                         snr=mean(sapply(a[start:end],function(x) mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))))
      b
    }
    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)
  } else {
    dflist=list()
    for (i in 1:(interval*times)) {
      inte=expand.grid(1:interval,1:times)$Var1[i]
      time=expand.grid(1:interval,1:times)$Var2[i]
      a=condextr(data,start=start,end=end,group=group,interval=inte,times=time,top=top,top.error=top.error,top.magnitude=top.magnitude,
                 bottom=bottom,bottom.error=bottom.error,bottom.magnitude=bottom.magnitude,by=by,half=half,cores=cores)
      b=cbind.data.frame(case=i,interval=inte,times=time,sdr=1-nrow(a)/nrow(data),orr=(sum(is.na(a[start:end]))-sum(is.na(data[data$date%in%a$date,start:end])))/(nrow(data)*ncol(data)),
                         snr=mean(sapply(a[start:end],function(x) mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))))
      dflist[[i]]=b
    }
    c=do.call(rbind,dflist)
  }
  c=transform(c,index=(exp(snr)-2)*10/exp(sdr+orr))
  c=transform(c,relaindex=index/log(interval*times+1))
  d=percoutl(data,start=start,end=end,group=group,top=top,bottom=bottom,by=by,half=half,cores=cores)
  e=cbind.data.frame(sdr=1-nrow(d)/nrow(data),orr=(sum(is.na(d[start:end]))-sum(is.na(data[data$date%in%d$date,start:end])))/(nrow(data)*ncol(data)),
                     snr=mean(sapply(d[start:end],function(x) mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))))
  c=transform(c,optimal=ifelse(sdr<e$sdr&orr<e$orr&snr>e$snr,T,F))
  cat(paste0('Time used by optisolu: ',format(Sys.time()-t0,digits=3)),'\n')
  c
}

shorvalu=function(data,start,end,intervals=30,units='mins') {
  t0=Sys.time()
  a=transform(data,period=cumsum(ifelse(c(0,as.numeric(diff(date),units=units))>intervals|c(0,as.numeric(diff(date),units=units))==0,1,0)),check.names=FALSE)
  if(length(find.package(c('dplyr','zoo'),quiet=TRUE))==2) {
    b=dplyr::mutate_at(dplyr::group_by(a,period),start:end,function(x) zoo::na.approx(x,rule=2))
    c=data.frame(b,check.names=FALSE)[1:end]
    d=cbind(c[1:end],data[setdiff(names(data),names(c[1:end]))])
    cat(paste0('Missing values left in selected variables: ',sum(is.na(c[start:end]))),'\n')
    cat(paste0(sum(is.na(data[start:end]))-sum(is.na(c[start:end])),' missing values are replaced by shorvalu interpolation'),'\n')
    cat(paste0('Time used by shorvalu: ',format(Sys.time()-t0,digits=3)),'\n')
    d
  } else {
    for (i in 1:length(unique(a$period))) {
      b=a[a$period==i,start:end]
      c=colSums(!is.na(b))
      a[a$period==i,start:end][c>=2]=lapply(b[c>=2],
                                            function(x) approx(x,xout=1:length(x),rule=2)$y)
      a[a$period==i,start:end][c==1]=lapply(b[c==1],
                                            function(x) x[!is.na(x)])
    }
    b=cbind(a[1:end],data[setdiff(names(data),names(a[1:end]))])
    cat(paste0('Missing values left in selected variables: ',sum(is.na(a[start:end]))),'\n')
    cat(paste0(sum(is.na(data[start:end]))-sum(is.na(a[start:end])),' missing values are replaced by shorvalu interpolation'),'\n')
    cat(paste0('Time used by shorvalu: ',format(Sys.time()-t0,digits=3)),'\n')
    b
  }
}

dataprep=function(data,start=NULL,end=NULL,group=NULL,optimal=FALSE,interval=10,times=10,fraction=.25,top=.995,top.error=.1,top.magnitude=.2,bottom=.0025,bottom.error=.2,bottom.magnitude=.4,by='min',half=30,intervals=30,cores=NULL) {
  t0=Sys.time()
  a=varidele(data,start=start,end=end,fraction=fraction)
  if(is.null(group)) {
    group=NULL
  } else if(group<start) {
    group=group
  } else {
    group=group-(ncol(data)-ncol(a))
  }
  end=end-(ncol(data)-ncol(a))
  b=obsedele(a,start=start,end=end,group=group,by=by,half=half,cores=cores)
  if(optimal==FALSE) {
    interval=interval
    times=times
  } else {
    s=subset(optisolu(b,start=start,end=end,group=group,interval=interval,times=times,top=top,top.error=top.error,top.magnitude=top.magnitude,
                      bottom=bottom,bottom.error=bottom.error,bottom.magnitude=bottom.magnitude,by=by,half=half,cores=cores),optimal==TRUE)
    if(!is.null(s)) {
      interval=s[s$relaindex==max(s$relaindex),]$interval
      times=s[s$relaindex==max(s$relaindex),]$times
    } else {
      stop('Please increase interval or/and times')
    }
  }
  c=condextr(b,start=start,end=end,group=group,interval=interval,times=times,top=top,top.error=top.error,top.magnitude=top.magnitude,
             bottom=bottom,bottom.error=bottom.error,bottom.magnitude=bottom.magnitude,by=by,half=half,cores=cores)
  d=shorvalu(c,start=start,end=end,intervals=intervals)
  if(optimal==TRUE) cat(paste0('Optimal solution is found when interval = ',interval,' and times = ',times),'\n')
  cat(paste0('Time used by dataprep: ',format(Sys.time()-t0,digits=3)),'\n')
  d
}
