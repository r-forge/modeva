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

<body bgcolor=#CCF5EB>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<font face="helvetica, verdana, arial"> 

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p> It includes functions for <b>variation partitioning</b>, assessing the <b>false discovery rate</b> and the level of <b>multicollinearity</b> among variables, calculating several measures of <b>model discrimination and calibration</b>, <b>optimizing prediction thresholds</b> based on a number of criteria, performing multivariate environmental similarity surface (<b>MESS</b>) <b>analysis</b>, and displaying various <b>analytical plots</b>. Includes also a <b>sample data set</b> with species occurrences and predictor variables.</p>
<br />


<h2> Install and load </h2>

<p>To <strong>install</strong> <i>modEvA</i> directly form R-Forge, paste the following command in the R console (when connected to the internet):</p>
<code>install.packages("modEvA", repos="http://R-Forge.R-project.org")</code><br />

<p>If the command above fails, producing a message like "<i>package 'modEvA' is not available for your R version</i>", you can either upgrade R to the latest version <i>or</i> download the compressed package source files to your disk (<i>.zip</i> or <i>.tar.gz</i> available <a href="https://r-forge.r-project.org/R/?group_id=1876">here</a>) and then install the package from there (R menu "<i>Packages - Install packages from local zip files</i>", or "<i>Tools - Install packages - Install from: Package Archive File</i>", or "<i>Packages & Data - Package installer, Packages repository - Local binary package</i>", ... depending on your R interface).</p>

<p>You only need to install the package once, but then every time you re-open R you need to <strong>load</strong> it by typing:</p>
<code>library(modEvA)</code><br />

<p>You can then check out the package <strong>help</strong> files and try some of the provided <strong>examples</strong>:</p>
<code>help("modEvA")</code><br /><br />


<h2> References </h2>

<h3> Citation of the <i>modEvA</i> package:</h3>

<p>Barbosa A.M., Brown J.A., Jim&eacute;nez-Valverde A. & Real R. (2014) modEvA: Model Evaluation and Analysis. R package, version 0.7</p>

<p><small><i>NOTE: To get the citation for the version currently installed on your system, load the <i>modEvA</i> package in R and then type </i><strong><code></small>citation(package="modEvA")</code></strong></p>

<h3> Papers introducing <i>modEvA</i>:</h3>

<p>Barbosa A.M., Real R., Mu&ntilde;oz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. <i>Diversity and Distributions</i> 19: 1333-1338 (DOI: <a href="http://onlinelibrary.wiley.com/doi/10.1111/ddi.12100/abstract">10.1111/ddi.12100</a>)</p>

<p>Barbosa A.M.,  Brown J.A., Jim&eacute;nez-Valverde A., Acevedo P., Lobo J.M. & Real R. (in prep.) The ABC of model evaluation: a visual method for a clearer assessment of model accuracy</p>

<br />

<h2>Find out more</h2>
<p> A <b><a href="modEvA-manual.pdf">reference <big>manual</big></a></b> based on the package help files is available in PDF format.</p>
<p> Click <a href="http://modtools.wordpress.com/packages/modeva/">here</a> for <b>further info</b> on the package and its origins. </p>
<p> The R-Forge project summary page you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">here</a>. </p>

 </font> 
</body>
</html>
