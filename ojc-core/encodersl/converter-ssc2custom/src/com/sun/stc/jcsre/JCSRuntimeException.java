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
 * @(#)JCSRuntimeException.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

import java.io.*;

/**
 * Base class for all Java Collaboration Service (JCS) Runtime exceptions.
 */
public class JCSRuntimeException extends RuntimeException
{

  private Throwable rootCause = null;

  /**
   * Constructs a generic <code>JCSRuntimeException</code> object.
   */
  public JCSRuntimeException()
  {
    super();
  }
  
  /**
   * Constructs a <code>JCSRuntimeException</code> object with text.
   * 
   * @param     s     the text for the exception.
   */
  public JCSRuntimeException(String s)
  {
    super(s);
  }
  
  /**
   * Constructs a <code>JCSRuntimeException</code> object with text and
   * root cause.
   * 
   * @param     cmt     the text for the exception.
   * @param     e       the root cause exception.
   */
  public JCSRuntimeException(String cmt, Exception e)
  {
    super(cmt);
    this.rootCause = e;
  }
  
  /**
   * Constructs a <code>JCSRuntimeException</code> object with text and
   * root cause.
   * 
   * @param     cmt     the text for the exception.
   * @param     t       the root cause exception.
   */
  public JCSRuntimeException(String cmt, Throwable e)
  {
    super(cmt);
    this.rootCause = e;
  }

  /**
   * Constructs a <code>JCSRuntimeException</code> object with a root cause
   * 
   * @param _e the root cause exception.
   */
  public JCSRuntimeException(Exception _e) 
  {
    super(_e.getMessage());
    this.rootCause = _e;
  }

  /**
   * Constructs a <code>JCSRuntimeException</code> object with a root cause
   * 
   * @param t   the root cause exception.
   */
  public JCSRuntimeException(Throwable t) 
  {
    super(t.getMessage());
    this.rootCause = t;
  }

  /**
   * Retrieves the stack trace of an exception as a string.
   * 
   * @param     e      the originating exception.
   * @return    the stack trace.
   */
  public static String getStackTrace(Exception e)
  {
    return getStackTrace((Throwable) e);
  }
 
  /**
   * Retrieves the stack trace of an exception as a string.
   * 
   * @param     e      the originating exception.
   * @return    the stack trace.
   */
  public static String getStackTrace(Throwable t)
  {
    ByteArrayOutputStream baos = new ByteArrayOutputStream();
    PrintStream ps = new PrintStream(baos);
    t.printStackTrace(ps);
    ps.flush();
    return baos.toString();
  }
 
  /** 
   * get the root cause exception
   * 
   * @return the root cause exception
   */
  public Exception getRootCause()
  {
    return (this.rootCause instanceof Exception) ? ((Exception) this.rootCause) : null;
  }
  
  /** 
   * get the root cause exception
   * 
   * @return the root cause exception
   */
  public Throwable getTargetException()
  {
    return this.rootCause;
  }

  /**
   * @see java.lang.Exception#printStackTrace()
   */
  public void printStackTrace() 
  {
    if (null != this.rootCause) this.rootCause.printStackTrace();
    super.printStackTrace();
  }

  /**
   * @see java.lang.Exception#printStackTrace(PrintStream)
   */
  public void printStackTrace(PrintStream _s) 
  {
    if (null != this.rootCause) this.rootCause.printStackTrace(_s);
    super.printStackTrace(_s);
  }

  /**
   * @see java.lang.Exception#printStackTrace(PrintWriter)
   */
  public void printStackTrace(PrintWriter _w) 
  {
    if (null != this.rootCause) this.rootCause.printStackTrace(_w);
    super.printStackTrace(_w);
  }
  
}
