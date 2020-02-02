<?php 
$errors = '';
$myemail = 'wedding@souravsengupta.com';

if(empty($_POST['Name']) || empty($_POST['E-mail']) || empty($_POST['Message']))
{
    $errors .= "\n Error: all fields are required";
}

$name = $_POST['Name']; 
$email_address = $_POST['E-mail']; 
$message = $_POST['Message']; 

if (!preg_match("/^[_a-z0-9-]+(\.[_a-z0-9-]+)*@[a-z0-9-]+(\.[a-z0-9-]+)*(\.[a-z]{2,3})$/i", $email_address))
{
    $errors .= "\n Error: Invalid email address";
}

if( empty($errors))
{
	$to = $myemail; 
	$email_subject = "Wedding message from $name";
	$email_body = "Name: $name \nEmail: $email_address \n\nMessage: \n$message"; 
	
	$headers = "From: $email_address\n"; 
	$headers .= "Reply-To: $email_address";
	
	mail($to,$email_subject,$email_body,$headers);

	header('Location: Guestbookty.html');
} 
?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<title>Sananda weds Sourav</title>
<meta charset="utf-8">
<link rel="stylesheet" href="css/reset.css" type="text/css" media="all">
<link rel="stylesheet" href="css/layout.css" type="text/css" media="all">
<link rel="stylesheet" href="css/style.css" type="text/css" media="all">
<script type="text/javascript" src="js/jquery-1.4.2.js" ></script>
<script type="text/javascript" src="js/cufon-yui.js"></script>
<script type="text/javascript" src="js/cufon-replace.js"></script>
<script type="text/javascript" src="js/Myriad_Pro_300.font.js"></script>
<!--[if lt IE 9]>
	<script type="text/javascript" src="http://info.template-help.com/files/ie6_warning/ie6_script_other.js"></script>
	<script type="text/javascript" src="js/html5.js"></script>
<![endif]-->
</head>
<body id="page1">
<div class="extra">
	<div class="main">
<!-- header -->
		<header>
			<nav>
				<ul id="menu">
					<li><a href="index.html">Home</a></li>
					<li><a href="Story.html">Our Story</a></li>
					<li><a href="Photos.html">Photos</a></li>
					<li id="menu_active" class="bg_none"><a href="Guestbook.html">Guestbook</a></li>
				</ul>
			</nav>
		</header>
<!-- / header -->
<!-- content -->
		<section id="content">
			<article class="col1">
				<h2><span>Sorry</span> - Message not sent!</h2>

				<table align="center" width="80%">
				<tr><td>
				It seems that there were some errors.
				<?php
				echo nl2br($errors);
				?>
				</td><td width="5%"></td><td>
				<a href="Guestbook.html" class="button">Please try again</a>
				</td></tr>
				</table>

				<br />

				<h2>You may also reach us directly</h2>
				<br />

				<table align="center" width="100%">
				<tr><td>
				<div class="wrapper">
					<figure class="left marg_right1"><img src="images/pushi.jpg" alt=""></figure>
					<p class="pad_bot1 pad_top1"><strong>The Bride</strong></p>
					<p>+91 94344 27083 (M)<br />me.sandy2@gmail.com</p>
				</div>
				</td><td width="5%"></td><td>
				<div class="wrapper">
					<figure class="left marg_right1"><img src="images/po.jpg" alt=""></figure>
					<p class="pad_bot1 pad_top1"><strong>The Groom</strong></p>
					<p>+91 94323 44852 (M)<br />sg.sourav@gmail.com</p>
				</div>
				</td></tr>
				</table>
			</article>

		</section>
<!-- / content -->
	</div>
</div>

<script type="text/javascript"> Cufon.now(); </script>
</body>
</html>
