/*
 * BEGIN_HEADER - DO NOT EDIT
 * 
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://open-jbi-components.dev.java.net/public/CDDLv1.0.html.
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with
 * your own identifying information: Portions Copyright
 * [year] [name of copyright owner]
 */

/*
 * @(#)BooleanFormat.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

import java.text.Format;
import java.text.FieldPosition;
import java.text.ParsePosition;
import java.text.ParseException;
import java.util.regex.Pattern;
import java.io.UnsupportedEncodingException;

import com.sun.stc.jcsre.IStringCoder;
import com.sun.stc.jcsre.StringCoderFactory;

/**
 * This class is analogous to java.text.DecimalFormat and
 * java.text.SimpleDateFormat, but it formats booleans.
 *
 * The format for the pattern is four fields separated by semicolons (;).
 *
 * The 1st field contains the regular expression used to detect true values.
 * The 2nd field contains the regular expression used to detect false values.
 * The 3rd field contains the string used to render true values.
 * The 4th field contains the string used to render false values.
 *
 * @see java.text.Format
 * @see java.text.NumberFormat
 * @see java.text.DecimalFormat
 */
public class BooleanFormat extends Format {

  public static final boolean unitTest = false;

  /**
   * Character used to separate pattern parts.
   */
  public static final char SEP_CHAR = ';';

  /**
   * Character used to escape the separator.
   */
  public static final char ESCAPE_CHAR = '\\';

  /**
   * java.text.FieldPosition value.
   */
  public static final int BOOLEAN_FIELD = 0;

  /**
   * Regular expression to match "true" values.
   */
  private Pattern trueRegex = null;

  /**
   * Regular expression to match "false" values.
   */
  private Pattern falseRegex = null;

  /**
   * String to represent "true".
   */
  private String trueString = null;

  /**
   * String to represent "false".
   */
  private String falseString = null;

  /** 
   * Construct an instance that uses the specified pattern
   * to parse and format boolean values.
   *
   * @param _pattern the pattern to use
   *
   * @throws IllegalArgumentException _pattern is null or invalid
   */
  public BooleanFormat (String _pattern) throws IllegalArgumentException {

    if(null == _pattern) {
      throw new IllegalArgumentException("Pattern cannot be null.");
    }

    setPattern(_pattern);
  }

  /**
   * Sets the pattern used by the formatter to parse/render booleans.
   *
   * @param _pattern the pattern
   *
   * @throws IllegalArgumentException _pattern is invalid
   */
  private void setPattern (String _pattern) throws IllegalArgumentException {
    
    try {

      int from = 0;
      int to = 0;
      String part;

      // create trueRegex
      to = getEndPos(_pattern, from);
      part = _pattern.substring(from,to);
      trueRegex = Pattern.compile(part);
      from = to+1;

      // false truePattern
      to = getEndPos(_pattern, from);
      part = _pattern.substring(from,to);
      falseRegex = Pattern.compile(part);
      from = to+1;

      // create trueString
      to = getEndPos(_pattern, from);
      trueString = _pattern.substring(from,to);
      from = to+1;

      // false trueString
      falseString = _pattern.substring(from);

    } catch(Exception ex) {
      ex.printStackTrace();

      // cleanup
      trueRegex = null;
      falseRegex = null;
      trueString = null;
      falseString = null;

      throw new IllegalArgumentException("Invalid pattern.");

    }

  }

  /**
   * Gets the position of the last character before the separator
   * taking into account that the separator may be escaped.
   *
   * @param _string the string to search
   * @param _from where to start looking from
   *
   * @return the position of the last character before the separator
   */
  private static int getEndPos (String _string, int _from) {

    int length = _string.length();
    int to = 0;
    boolean escaped = false;

    for(to=_from; to<length-1 ;to++) {
      switch(_string.charAt(to)) {

        case ESCAPE_CHAR:
          escaped = !escaped;
          break;

        case SEP_CHAR:
          if(!escaped)
            return to;
          // do default too

        default:
          escaped = false;
          break;

      }
    }

    return to;
  }

  /**
   * Formats a boolean value based on the pattern provided at construction.
   *
   * @param _obj object to format
   * @param _buffer buffer to place text in
   * @param _pos field position for the resulting string
   * 
   * @return _buffer
   *
   * @throws IllegalArgumentException unable to format specified object
   *
   * @see java.text.Format#format(Object, StringBuffer, FieldPosition)
   */
  public StringBuffer format (Object _obj
                            ,StringBuffer _buffer
                            ,FieldPosition _pos
  ) throws IllegalArgumentException {

    if(!(_obj instanceof Boolean)) {
      throw new IllegalArgumentException("Object is not of type java.lang.Boolean.");
    }
    boolean value = ((Boolean)_obj).booleanValue();

    if(null == _buffer) {
      throw new IllegalArgumentException("buffer is null.");
    }

    if(null != _pos && BOOLEAN_FIELD == _pos.getField()) {
      _pos.setBeginIndex(_buffer.length());
    }

    _buffer.append(value?trueString:falseString);

    if(null != _pos && BOOLEAN_FIELD == _pos.getField()) {
      _pos.setEndIndex(_buffer.length());
    }

    return _buffer;
  }

  /**
   * Formats a boolean primitive value.
   *
   * @param _value the value
   *
   * @return a string containing the formatted value
   */
  public String format (boolean _value) {
    return format(new Boolean(_value)).toString();
  }

    /**
     * Formats the specified boolean into a byte array with specified coder.
     *
     * @param _value the value to format
     * @param _coder coder to use
     *
     * @return byte array containing formatted number
     */
    public byte[] format (boolean _value, IStringCoder _coder) {
      return _coder.encode(format(_value));
    }
    /**
     * Formats the specified boolean into a byte array with specified encoding.
     *
     * @param _value the value to format
     * @param _enc encoding to use
     *
     * @return byte array containing formatted number
     *
     * @throws UnsupportedEncodingException _enc bad
     */
    public byte[] format (boolean _value, String _enc)
      throws UnsupportedEncodingException
    {
      return format(_value, StringCoderFactory.getStringCoder(_enc));
    }

  /**
   * Parses a java.lang.Boolean Object based on the pattern provided
   * at construction.
   *
   * @param _source the source string
   * @param _pos the parse position (ignored)
   *
   * @return the parsed object or null if an error occurred
   */
  public Object parseObject (String _source, ParsePosition _pos) {

    // make sure wwe set the index or parseObject(String) will throw exception
    if(null != _pos) {
      _pos.setIndex(_source.length());
    }

    if(trueRegex.matcher(_source).find()) return Boolean.TRUE;
    if(falseRegex.matcher(_source).find()) return Boolean.FALSE;

    return null;
  }

  /**
   * Parses a boolean primitive based on the pattern provided at construction.
   *
   * @param _value string representation of the boolean value
   *
   * @return  the boolean value
   *
   * @throws ParseException unable to parse
   */
  public boolean parse (String _source) throws ParseException {
    try {
      return ((Boolean)parseObject(_source)).booleanValue();
    } catch(Exception ex) {
      if( ex instanceof ParseException) {
        throw (ParseException)ex;
      }
      throw new ParseException("Unable to parse boolean.", 0);
    }
  }

  /**
   * Parses a boolean primitive from a byte array and it's coder
   *
   * @param _bytes byte array containing the string representation of boolean value
   * @param _coder the string coder for the byte array
   *
   * @return  the boolean value
   *
   * @throws ParseException unable to parse
   */
  public boolean parse (byte[] _bytes, IStringCoder _coder)
      throws ParseException
  {
      return parse(_coder.decode(_bytes));
  }

  /**
   * Parses a boolean primitive from a byte array and it's encoding
   *
   * @param _bytes byte array containing the string representation of boolean value
   * @param _enc encoding of the byte array
   *
   * @return  the boolean value
   *
   * @throws ParseException unable to parse
   * @throws UnsupportedEncodingException _enc bad
   */
  public boolean parse (byte[] _bytes, String _enc)
      throws ParseException, UnsupportedEncodingException
  {
      return parse(_bytes, StringCoderFactory.getStringCoder(_enc));
  }

  /**
   * Converts the entire class state to a string.
   *
   * @return a string containing a human readable version of the class
   */
  @Override
  public String toString () {
    return getClass().getName() + "[trueRegex: " + trueRegex
          +" falseRegex: " + falseRegex
          +" trueString: " + trueString
          +" falseString: " + falseString
          + "]"
          ;

  }

  /**
   * The "main" method for testing purposes.
   * Usage is: BooleanFormat [pattern] [string...]
   *
   * @param _args  the command-line arguments
   */
  public static void main (String[] _args) {
    if(!BooleanFormat.unitTest) return;
    
    if(_args.length<2) {
      System.out.println("Must pass pattern and list of strings to format.");
      System.exit(1);
    }
  
    try {
      BooleanFormat formatter = new BooleanFormat(_args[0]);
      System.out.println("formatter: " + formatter);

      for(int ii=1; ii<_args.length ;ii++) {
        String arg = _args[ii];

        try {
          System.out.println("parsing: " + arg);
          boolean bool = formatter.parse(arg);
          System.out.println("parsed as: " + bool);
          String str = formatter.format(bool);
          System.out.println("rendered as: " + str);
        } catch(Exception ex) {
          ex.printStackTrace();
        }

      }

    } catch(Exception ex) {
      ex.printStackTrace();
    }
  }
}
