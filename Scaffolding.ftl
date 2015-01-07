<!DOCTYPE html>
<!--[if lt IE 7 ]><html class="ie ie6" lang="en"> <![endif]-->
<!--[if IE 7 ]><html class="ie ie7" lang="en"> <![endif]-->
<!--[if IE 8 ]><html class="ie ie8" lang="en"> <![endif]-->
<!--[if (gte IE 9)|!(IE)]><!--><html lang="en"> <!--<![endif]-->
<head>

  <meta charset="utf-8" />
  <title>WASH Info - Powered by ActivityInfo</title>
  <meta name="description" content="WASH Info - Powered by ActivityInfo">
  <meta name="viewport" content="width=device-width; initial-scale=1; maximum-scale=1">

  <!--[if lt IE 9]>
  <script src="//html5shim.googlecode.com/svn/trunk/html5.js"></script>
  <![endif]-->


  <link rel="stylesheet" href="https://bedatadriven.github.io/activityinfo-washinfo/css/site.css">
  <link rel="stylesheet" href="https://bedatadriven.github.io/activityinfo-washinfo/css/prettify.css">
  <link href="//www.activityinfo.org/favicon.ico" rel="icon" type="image/x-icon">
  
  <link rel="stylesheet" href="https://bedatadriven.github.io/activityinfo-washinfo/css/font-humanitarian.css">
  <!--[if IE 7]>
  <link rel="stylesheet" href="https://bedatadriven.github.io/activityinfo-washinfo/css/font-humanitarian-ie7.min.css">
  <![endif]-->
  <!-- Le fav and touch icons -->
</head>


<body data-spy="scroll" data-target=".navbar" style="margin-top:60px">

    <div class="navbar navbar-inverse navbar-fixed-top">
      <div class="navbar-inner">
        <div class="container">
          <a class="brand" href="#">WASH Info</a>
          <ul class="nav">
            <li><a href="/#why">Why?</a></li>
            <li><a href="/#what">What?</a></li>
            <li><a href="/#who">Who?</a></li>
            <li><a href="https://about.activityinfo.org/learn/ai201/lesson-1-introduction-to-the-course/" target="_blank">How?</a></li>

          </ul>

        </div>
      </div>
    </div>
    
    
  ${body}


    <footer>
      <div class="container">
        <div class="row">
          <div class="span4">
          <div class="span8">
            <h2>Credits</h2>
 
            <p>Developed as part of IRC's <a href="http://www.ircwash.org/projects/brac-wash-ii-dgis-and-bmgf">BRAC WASH II programme</a>, with the support of the Netherlands <a href="government.nl/ministries/bz">Ministry of Foreign Affairs</a> and the 
            <a href="#">Bill and Melinda Gates Foundation</a></p>
            <p><a class="brand" href="https://about.activityinfo.org" style="" target="_blank"><img src="https://bedatadriven.github.io/activityinfo-washinfo/img/logo-shadow.png"><span> About ActivityInfo</span></a></p>
             <p>ActivityInfo is developed by <a href="http://www.bedatadriven.com" target="_blank">BeDataDriven</a>.</p>
            </p>
          </div>
        </div>
      </div>
    </footer>

<script src="https://bedatadriven.github.io/activityinfo-washinfo/js/jquery-1.7.1.min.js"></script>
<script src="https://bedatadriven.github.io/activityinfo-washinfo/js/underscore.min.js"></script>
<script src="https://bedatadriven.github.io/activityinfo-washinfo/js/backbone.min.js"></script>
<script src="https://bedatadriven.github.io/activityinfo-washinfo/js/prettify.min.js"></script>
<script src="https://bedatadriven.github.io/activityinfo-washinfo/js/bootstrap-222.min.js"></script>
<script src="https://bedatadriven.github.io/activityinfo-washinfo/js/index/index.js"></script>

<script type="text/javascript">		
	var enableForm = function(enabled) {
		$('#loginButton').prop('disabled', !enabled);
		$('#loginSpinner').toggleClass('hide', enabled);
	}	

	$('#loginForm').submit(function() {
		
		$('#loginAlert').addClass('hide');
	
		enableForm(false);		
		$.ajax({
			url: '/login/ajax',
			type: 'POST', 
			data: {
				email: $('#emailInput').val(),
				password: $('#passwordInput').val(),
				ajax: 'true'
			},
			success: function() {
				if(window.location.pathname != '/') {
					window.location = '/' + window.location.search + window.location.hash;
				} else {
					window.location.reload(true);
				}
			},
			error: function(xhr) {
				$('#loginAlert').toggleClass('hide', false);
			},
			complete: function() {
				enableForm(true);
			}
		});
		return false;
	});
	
	$('#emailInput').focus();
</script>



</body>
</html>

