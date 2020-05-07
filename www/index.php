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

<body bgcolor=#FFFFFF>

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

<p> It includes functions for <b>variation partitioning</b>; calculating several measures of model <b>discrimination</b>, <b>classification</b>, <b>explanatory power</b>, and <b>calibration</b>; <b>optimizing prediction thresholds</b> based on a number of criteria; performing multivariate environmental similarity surface (<b>MESS</b>) <b>analysis</b>; and displaying various <b>analytical plots</b>. It includes also a <b>sample data set</b> with some species distribution models.</p>

<font color="green">
<h3> Package <i>modEvA</i> (v2.0) is on <a href="https://CRAN.R-project.org/package=modEvA">CRAN</a>!</h3>
</font>

<h2> Install and load </h2>

<p>To <strong>install</strong> the currently <b>stable version</b> of <i>modEvA</i> from CRAN, paste the following command in the R console (while connected to the internet):</p>

<code>install.packages("modEvA")</code><br />


<p>To install a <b>developing version</b> (with new features, but possibly also new bugs) of <i>modEvA</i> from R-Forge, paste the following command in the R console (while connected to the internet):</p>

<code>install.packages("modEvA", repos="http://R-Forge.R-project.org")</code><br />

<font color="grey" size="1">
<p>This should work if you have the <b>latest version of R</b>; otherwise, it may either fail (producing a message like "<i>package 'modEvA' is not available for your R version</i>") or install an older version of <i>modEvA</i>. To <b>check the <i>modEvA</i> version that you have actually installed</b>, type <big><b><code>citation(package="modEvA")</code></b></big>. To install the latest <i>modEvA</i> version, you can either upgrade R <i>or</i> download the compressed <i>modEvA</i> <b>package source files</b> to your disk (<i>.tar.gz</i> for Linux/Mac or <i>.zip</i> for Windows, <b>available <a href="https://r-forge.r-project.org/R/?group_id=1876">here</a> or <a href="https://www.dropbox.com/sh/oac92wu1dbfsol4/AAAKVs4oVBBCUtDkPTnbh11Ga?dl=0">here</a></b>) and then install the package from there, e.g. with R menu "<i>Packages - Install packages from local zip files</i>" (Windows), or "<i>Packages & Data - Package installer, Packages repository - Local source package</i>" (Mac), or "<i>Tools - Install packages - Install from: Package Archive File</i>" (RStudio).</p>
</font>

<p>You only need to install (each version of) the package once, but then every time you re-open R you need to <strong>load</strong> the package by typing:</p>
<code>library(modEvA)</code><br />

<p>You can then check out the package <strong>help</strong> files and try some of the provided <strong>examples</strong>:</p>
<code>help("modEvA")</code><br /><br />


<h2> References </h2>

<h3> Papers introducing <i>modEvA</i>:</h3>

<p>Barbosa A.M., Real R., Mu&ntilde;oz A.R. & Brown J.A. (2013) New measures for assessing model equilibrium and prediction mismatch in species distribution models. <i>Diversity and Distributions</i> 19: 1333-1338 (DOI: <a href="http://onlinelibrary.wiley.com/doi/10.1111/ddi.12100/abstract">10.1111/ddi.12100</a>)</p>

<p>Areias-Guerreiro J., Mira A. & Barbosa A.M. (2016) How well can models predict changes in species distributions? A 13-year-old otter model revisited. <i>Hystrix – Italian Journal of Mammalogy, 27(1). DOI: https://doi.org/10.4404/hystrix-27.1-11867</i></p>

<p>Barbosa A.M., Brown J.A., Acevedo P., Lobo J.M. & Real R. (in prep.) The ABC of model evaluation: a visual method for a clearer assessment of model accuracy</p>


<h3> Other papers using <i>modEvA</i> (excluding self-citations):</h3>

<p>Coelho L., Romero D., Queirolo D. & Guerrero J.C. (2018) Understanding factors affecting the distribution of the maned wolf (<i>Chrysocyon brachyurus</i>) in South America: Spatial dynamics and environmental drivers. <i>Mammalian Biology</i>, 92: 54-61. https://doi.org/10.1016/j.mambio.2018.04.006</p>

<p>De Ara&uacute;jo C.B., Marcondes-Machado L.O. & Costa G.C. (2013) The importance of biotic interactions in species distribution models: a test of the Eltonian noise hypothesis using parrots. <i>Journal of Biogeography</i>, 41: 513-523</p>

<p>Dias A., Palma L., Carvalho F., Neto D., Real J., Beja P. (2017) The role of conservative versus innovative nesting behavior on the 25‐year population expansion of an avian predator. Ecology and Evolution 7: 4241–4253. https://doi.org/10.1002/ece3.3007</p>

<p>Horn, J., Becher, M. A., Kennedy, P. J., Osborne, J. L. & Grimm, V. (2016) Multiple stressors: using the honeybee model BEEHAVE to explore how spatial and temporal forage stress affects colony resilience. <i>Oikos</i>, 125: 1001-1016. DOI:10.1111/oik.02636</p>

<p>Moreno‐Zarate L., Estrada A., Peach W. & Arroyo B. (2020) Spatial heterogeneity in population change of the globally threatened European turtle dove in Spain: The role of environmental favourability and land use. Diversity & Distributions, https://doi.org/10.1111/ddi.13067</p>

<p>Naman S.M., Rosenfeld J.S., Kiffney P.M., Richardson J.S. (2018) The energetic consequences of habitat structure for forest stream salmonids. <i>Journal of Animal Ecology</i>, DOI: 10.1111/1365-2656.12845</p>

<p> Romero D., Olivero J., Real R. & Guerrero J.C. (2019) Applying fuzzy logic to assess the biogeographical risk of dengue in South America. <i>Parasites & Vectors</i>, 12: 428. DOI: 10.1186/s13071-019-3691-5</p>

<br />


<h2>Find out more</h2>

<p> Here's a quick <b><a href="modEvA-tutorial.html">illustrated <big>tutorial</big></a></b>, also available in <b><a href="modEvA-tutorial.pdf">PDF</a></b>; and a <b><a href="modEvA-manual.pdf">reference <big>manual</big></a></b> based on the package help files. There's also a <a href="http://modeva.r-forge.r-project.org/Guia_modelacion_fuzzySim_modEvA.html">course manual on model building with <i>fuzzySim</i> and model evaluation with <i>modEvA</i> (<b>in Spanish</b>)</a></p>

<p> Click <a href="http://modtools.wordpress.com/packages/modeva/">here</a> for <b>further info</b> on the package and its origins. </p>
<p> The R-Forge project summary page you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">here</a>. </p>

 </font> 
</body>
</html>
