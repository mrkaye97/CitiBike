## Exploring the New York City CitiBike data

## Plots

#### Visualizing Commute Patterns -- Heatmap of start and end stations by time of day
<img src="https://raw.githubusercontent.com/mrkaye97/CitiBike/master/viz/commutes.svg">


#### Coronavirus Impact on Ridership

<img src="https://raw.githubusercontent.com/mrkaye97/CitiBike/master/viz/coronavirus-overall-impact.svg">


#### Common Routes
<html> 
  <head> 
    <script src="jquery.js"></script> 
    <script> 
    $(function(){
      $("#includedContent").load("viz/common-routes.html"); 
    });
    </script> 
  </head> 

  <body> 
     <div id="includedContent"></div>
  </body> 
</html>