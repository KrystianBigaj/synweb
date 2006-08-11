----------------------------------------
- TSynWeb v1.1.61 Unicode/ANSI
----------------------------------------

Features:
- support for embedded:
  - PHP, CSS, JS in HTML
  - PHP in CSS, JS
- support for UniSynEdit
- full validation for tags (also checks for valid '/>' or '/') and its attributes for HTML across difference versions (for XHTML - case sensitive)
- values in tags without quotation are also highlighted as ValueAttrib (only in HTML, in XHTML is highlighted as error because, XHTML doesn't allow for unquoted values)
- in CSS validation for tags (you can set also HTML version)
- validation for Ampersand tags (eg. &amp;, &copy;)
- almost FULL validation for CSS across CSS1 and CSS2.1
- support for <script language="php"> as start tag for PHP (also <?, <?php, <?=, <%)
- end tags for PHP doesn't stop in strings, comments (stops only in singleline), etc, you can now write "<?xml ... ?>" and PHP mode doesn't stop,
- suppor for custom HEREDOC names (defined in TStringList, or any you type - comparing based on CRC8)
- supprt for encapusled vars or escaped chars in strings (different highlighter attrib) with error checking
- any word (but not keyword or function name) writed in UpperCase highlighted as ConstantAttrib
- ActiveHighlighterSwitch - see demo (check 'Active HL' in demo app)
- parsed source code of php to get function names (for php4 and php5+PECL)
- any many more.

----------------------------------------
- Installation
----------------------------------------

1. Put files in synedit sources (eg. \SynEdit\Source\)
   - SynHighlighterWeb.pas
   - SynHighlighterWebData.pas
   - SynHighlighterWebMisc.pas
   - QSynHighlighterWeb.pas
   - QSynHighlighterWebData.pas
   - QSynHighlighterWebMisc.pas

2. If you are using ANSI version of SynEdit (not UniSynEdit) then go to step 3.
Add to SynEdit.inc line:
{$DEFINE UNISYNEDIT}
(without it, TSynWeb working with ANSI version of SynEdit):

3. In SynEdit_RXXX.dpk add:
  SynHighlighterWeb in '..\SynHighlighterWeb.pas',
  SynHighlighterWebData in '..\SynHighlighterWebData.pas';

4. To uses in SynEditReg.pas add:
  SynHighlighterWeb,
  SynHighlighterWebData,
and at RegisterComponents(SYNS_HighlightersPage ... add this:
    TSynWebEngine, TSynWebHtmlSyn, TSynWebPhpCliSyn, TSynWebCssSyn, TSynWebEsSyn

5. Recompile design and runtime package.

Hint: Before using TSynWeb...Syn in TSynEdit add TSynWebEngine on form and set Engine property for TSynWeb...Syn to that TSynWebEngine

Remember: TSynWeb will NOT work with TSynMultiSyn

----------------------------------------
- Info
----------------------------------------

Homepage: http://flatdev.dotgeek.org
Email: krystian.bigaj@gmail.com
