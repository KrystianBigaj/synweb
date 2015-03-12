**Note: Please use code from SVN http://code.google.com/p/synweb/source/checkout, download page is outdated.**
Delphi 2009(Unicode) support, HTML5/CSS3, and other enhancements/fixes available in SVN (changelog: https://code.google.com/p/synweb/source/list)

SynWeb is an advanced highlighter for (Uni)SynEdit for PHP, (X)HTML, WML, CSS, JS. It has much more features than TSynMultiSyn and works about 2-10 times faster. Included also SynTokenMatch - inteligment brace/token matching, html tags, begin->end, etc.)

Some SynWeb features:
  * support for embedded:
    * PHP, CSS, JS in XHTML/HTML
    * PHP in CSS (.css), JS (.js), WML
  * support for ANSI and Unicode version of SynEdit
  * faster about 2-10 times than using TSynMultiSyn
  * full validation for tags (also checks for valid '/>' or '/') and its attributes for HTML across difference versions (for XHTML - case sensitive)
  * values in tags without quotation are also highlighted as ValueAttrib (only in HTML, in XHTML is highlighted as error because, XHTML doesn't allow for unquoted values)
  * support for WML 1.1/1.2/1.3
  * in CSS validation for tags (you can set also HTML version)
  * validation for special entity characters (eg. &amp;, &copy;)
  * almost FULL validation for CSS across CSS1 and CSS2.1
  * support for 

&lt;script language="php"&gt;

 as start tag for PHP (also <?, <?php, <?=, <%)
  * end tags for PHP doesn't stop in strings, comments (stops only in singleline), etc, you can now write "<?xml ... ?>" and PHP mode doesn't stop,
  * support for custom HEREDOC names (defined in TStringList, or any you type - comparing based on CRC8)
  * support for encapusled vars or escaped chars in strings (different highlighter attrib) with error checking
  * any word in PHP (but not keyword or function name) written in UpperCase highlighted as ConstantAttrib
  * ActiveHighlighterSwitch - see demo (check 'Active HL' in demo app)
  * parsed source code of php to get built-in function names (for php4 and php5+PECL)
  * any many more.
