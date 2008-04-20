----------------------------------------
- TSynWeb v1.5 Unicode/ANSI
----------------------------------------

Features:
    *  support for embedded:
          o PHP, CSS, JS in XHTML/HTML
          o PHP in CSS (.css), JS (.js), WML, XML
    * support for ANSI and Unicode version of SynEdit
    * faster about 2-10 times than using TSynMultiSyn
    * support for Smarty
    * full validation for tags (also checks for valid '/>' or '/') and its attributes for HTML across difference versions (for XHTML - case sensitive)
    * values in tags without quotation are also highlighted as ValueAttrib (only in HTML, in XHTML is highlighted as error because, XHTML doesn't allow for unquoted values)
    * support for WML 1.1/1.2/1.3
    * in CSS validation for tags (you can set also HTML version)
    * validation for special entity characters (eg. &amp;, &copy;)
    * almost FULL validation for CSS across CSS1 and CSS2.1
    * support for <script language="php"> as start tag for PHP (also <?, <?php, <?=, <%)
    * end tags for PHP doesn't stop in strings, comments (stops only in singleline), etc, you can now write "<?xml ... ?>" and PHP mode doesn't stop,
    * support for custom HEREDOC names (defined in TStringList, or any you type - comparing based on CRC8)
    * support for encapusled vars or escaped chars in strings (different highlighter attrib) with error checking
    * any word in PHP (but not keyword or function name) written in UpperCase highlighted as ConstantAttrib
    * ActiveHighlighterSwitch - see demo (check 'Active HL' in demo app)
    * parsed source code of php to get built-in function names (for php4 and php5+PECL)
    * function to fill data in TSynCompletionProposal form SynWeb data (for php/css/html)
    * any many more.


----------------------------------------
- Installation
----------------------------------------

1. Install SynEdit/UniSynEdit first.
   (If you installed SynWeb previously with modifying SynEdit release files (before v1.2.76),
   then recompile SynEdit with original release)

2. SynWeb is configured to work by default with Unicode version of SynEdit (UniSynEdit - http://mh-nexus.de/unisynedit.htm)
   If you want use SynWeb with ANSI version of SynEdit (http://synedit.sourceforge.net)
   then open file "Source\SynWeb.inc" and comment (or delete) line:
     {$DEFINE UNISYNEDIT}  // Enable SynWeb Unicode version ...

3. Add path to "\SynWeb\Source" in Library Path.
   Open from Delphi menu "Tools/Enviroment Options..."
   Select tab "Library" and click on [...] in "Library path".
   Add full path to "...\SynWeb\Source".
   Hint: Same path to SynEdit\Source should be added, if not then you get errors while compiling packages.

4. Open "SynWeb_RXXX.dpk" (where "XXX" is your version of Delphi) and Compile runtime package.
   Open "SynWeb_DXXX.dpk" and then Compile and Install design-time package.
   You should get message of installed five components:
     TSynWebEngine, TSynWebHtmlSyn, TSynWebCssSyn, TSynWebEsSyn, TSynWebPhpCliSyn, TSynWebWmlSyn, TSynWebXmlSyn

All SynWeb components will be avaiable at "SynEdit Highlighters" component tab.


----------------------------------------
- Informations
----------------------------------------

Before using TSynWebXXXSyn in TSynEdit add TSynWebEngine on form and set Engine property for TSynWebXXXSyn to that TSynWebEngine
TSynWebEngine is not highlighter but SynWeb highlighters uses it (it's main parsing component for SynWeb highlighters).



TSynMultiSyn notice:
  TSynWeb will NOT work properly with TSynMultiSyn.
  You can use it but only with custom ranges (you must handle events in TSynMultiSyn) and with some limitations
  (you cannot use SynWeb with HTML+PHP, because it uses all 32 avaiable bits).

  Range 4 bytes - 32 bits:
  &&&##### #######% %%%%%%%% %%%%%%%%

  % - bits used by HTML, CSS, ES/JS (0..16: 17 bits)
  # - bits used by PHP (17..28: 12 bits)
  & - bits used by HTML and PHP	(29..31: 3 bits)

  So for example if you want use HTML (with optional CSS, ES/JS, but PHP switched off),
  then you can use # (17..28: 12 bits) bits for other highlighters in SynMultiSyn.


TSynWebSmartySyn limitations:
  - Options.SmartyLDelim MUST begin with '{' and Options.SmartyRDelim MUST begin with '}'.
  - Smarty highlighter doesn't support {literal} ... {/literal}

----------------------------------------
- Donations
----------------------------------------

SynWeb is completely free, however, if you find it useful and would like to support future development,
any donation would be greatly appreciated. Thank you!

My PayPal account: krystian.bigaj@gmail.com
				
Donate: http://flatdev.ovh.org/donate.php


----------------------------------------
- AboutBox ;) 
----------------------------------------

Author: Krystian Bigaj
Email: krystian.bigaj@gmail.com
Homepage: http://flatdev.ovh.org

Project page: http://sourceforge.net/projects/synweb
License: LGPL or MPL
							