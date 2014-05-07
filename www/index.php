<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->



<p> It includes functions for <b>variation partitioning</b>, assessing the <b>false discovery rate</b>, calculating several measures of <b>model discrimination and calibration</b>, <b>optimizing prediction thresholds</b> based on a number of criteria, performing multivariate environmental similarity 
surface (<b>MESS</b>) <b>analysis</b>, and displaying various <b>analytical plots</b>. Includes also a <b>sample data set</b> with species occurrences and predictor variables.</p>

<br />

<h2> Install and load </h2>
<p>Paste the following commands in the R console (when connected to the internet):</p>
<code>
install.packages("modEvA", repos="http://R-Forge.R-project.org")<br />
library(modEvA)<br />
help("modEvA")
</code>
<br />
<p>If you bump into problems, please go <a href="http://modtools.wordpress.com/packages/modeva/">here</a> for more complete download information.</p>
<br />

<h2> References </h2>

<h3> Citation for the <i>modEvA</i> package:</h3>

<p>Barbosa A.M., Brown J.A., Jim&eacute;nez-Valverde A. & Real R. (2014) modEvA: Model Evaluation and Analysis. R package, version 0.3</p>

<p><small><i>NOTE: To get the citation for the version currently installed on your system, load the <i>modEvA</i> package <font color="darkgrey">[by typing</font> <code>library(modEvA)</code><font color="darkgrey"> in R]</font> and type <b><code>citation(package="modEvA")</code></b></i></small></p>

<h3> Papers introducing <i>modEvA</i>:</h3>

<p>Barbosa A.M., Real R., Mu&ntilde;oz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. <i>Diversity and Distributions</i> 19: 1333-1338 (DOI: <a href="http://onlinelibrary.wiley.com/doi/10.1111/ddi.12100/abstract">10.1111/ddi.12100</a>)</p>

<p>Barbosa A.M.,  Brown J.A., Jim&eacute;nez-Valverde A., Acevedo P., Lobo J.M. & Real R. (in prep.) The ABC of model evaluation: a visual method for a clearer assessment of model accuracy</p>

<br />

<h2>Find out more</h2>

<p> Click <a href="http://modtools.wordpress.com/packages/modeva/">here</a> for <b>further info</b> on the package, including some <b>user guidelines</b>. </p>

<p> The R-Forge project summary page you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">here</a>. </p>


</body>
</html>
</body>
</html>
</body>
</html>
