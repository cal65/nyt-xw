require(data.table)

group_months <- function(df, date_col='Puzzle_date'){
  df$Month <- month(df[[date_col]])
  df$Year <- year(df[[date_col]])
  df$Weekday <- df$`Day of Week`
  count_df <- df[, .(n=.N), by=c('Month', 'Year', 'Weekday')]
  return (count_df)
}

prep <- function(df){
  df$Puzzle_date <- as.Date(df$`Puzzle Date`)
  df$Minutes <- df$`Time Taken`/60
  df$Completed_at <- as.POSIXct(df$`Completed At (ET)` )
  df$`Day of Week` <- factor(df$`Day of Week` ,
                             levels = c('Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa', 'Su'))
  return(df)
}

plot_box <- function(df){
  max_minutes <- round(max(df$Minutes), -1)
  p <- ggplot(df) + geom_boxplot(aes(x=`Day of Week`, y=Minutes, fill=`Day of Week`)) +
    scale_fill_brewer(palette='Reds', guide="none") + 
    scale_y_continuous(breaks=seq(0, max_minutes, 10)) +
    expand_limits(y=0) +
    ggtitle("Crossword Times") +
    theme(plot.title = element_text(hjust=0.5))
  return(p)
}

plot_ts <- function(df, date_col='Puzzle_date', y_col='Minutes', y_high=180){
  max_minutes <- round(max(df[[y_col]]), -1)
  df <- df[get(y_col) < y_high]
  fastest <- merge(df[y_col>0.5,.(Minutes=min(.SD[[y_col]])), by = `Day of Week`], stats,
                   by = c('Day of Week', y_col))
  p <- ggplot(df, aes(x=get(date_col), y=get(y_col))) +
    geom_line(linetype='dashed') + geom_point(alpha=0.5) + geom_smooth(method='gam') +
    geom_point(data=fastest, shape=11, color='gold', size=1.5) +
    facet_grid(. ~ `Day of Week`) +
    expand_limits(y=0) + scale_y_continuous(breaks=seq(0, max_minutes, 10)) +
    scale_color_brewer(palette='Set1', guide=F) +
    scale_shape(guide=F) +
    xlab(gsub('_', ' ', date_col)) + ylab(y_col) + 
    ggtitle("Crossword Performance") +
    theme(plot.title = element_text(hjust=0.5),
          axis.text.x = element_text(angle=90))
  return(p)
}
