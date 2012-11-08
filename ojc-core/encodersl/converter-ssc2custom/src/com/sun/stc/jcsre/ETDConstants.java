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
 * @(#)ETDConstants.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcsre;

/**
 * An interface to define String and int constants used in the ETD interface.
 *
 * @see      com.sun.stc.jcsre.ETD
 * @version  $Revision: 1.1 $
 */
public interface ETDConstants {
  /**
   * ETD is used for input.
   */
  public final static int IN_MODE = 0;
  
  /**
   * ETD is used for output.
   */
  public final static int OUT_MODE = 1;
  
  /**
   * ETD is used for both input and output.
   */
  public final static int IN_OUT_MODE = 2;
  
  /**
   * Name of "initialize" method: initialize
   */
  public final String INITIALIZE = "initialize";
  
  /**
   * Name of "getKey" method: retrieveKey
   */
  public final static String GETKEY = "retrieveKey";
  
  /**
   * Name of "unmarshal" method: unmarshal
   */
  public final static String UNMARSHAL = "unmarshal";

  /**
   * Name of "getMode" method: retrieveMode
   */
  public final static String GETMODE = "retrieveMode";
  
  /**
   * Name of "getNextSrcEvent" method: next
   */
  public final static String GETNEXTSRCEVENT = "next";
  

  /**
   * Name of "terminate" method: terminate
   */
  public final static String TERMINATE = "terminate";
  
  /**
   * Name of "marshal" method: marshal
   */
  public final static String MARSHAL = "marshal";

  /**
   * Name of "iqPut" method: send
   */
  public final static String IQPUT = "send";

  /**
   * Name of "hasData" method: available
   */

  public final static String HASDATA = "available";

  /**
   * Name of "reset" method: reset
   */
  public final static String RESET = "reset";

  /**
   * Name of "iqGet" method: receive
   */
  public final static String IQGET = "receive";

  /**
   * Name of "rawInput" method: rawInput
   */
  public final static String RAWINPUT = "rawInput";

  /**
   * Name of "topic" method: topic
   */
  public final static String TOPIC = "topic";

  /**
   * Name of "publications" method: publications
   */
  public final static String PUBLICATIONS = "publications";

  /**
   * Name of "subscriptions" method: subscriptions
   */
  public final static String SUBSCRIPTIONS = "subscriptions";

  // Signatures to be used in XSC files.

  /**
   * Signature of "hasData" method: available()Z
   */
  public final static String HASDATA_SIG = HASDATA+"()Z";

  /**
   * Signature of "getNextSrcEvent" method: next()Z
   */
  public final static String GETNEXTSRCEVENT_SIG = GETNEXTSRCEVENT+"()Z";

  /**
   * Signature of "iqPut" method: send()V
   */
  public final static String IQPUT_SIG = IQPUT+"()V";

  /**
   * Signature of "iqPut" method: send(Ljava/lang/String;)V
   */
  public final static String IQPUT_SIG2 = IQPUT+"(Ljava/lang/String;)V";

  /**
   * Signature of "iqGet" method: receive()Z
   */
  public final static String IQGET_SIG = IQGET+"()Z";

  /**
   * Signature of "iqGet" method: receive(Ljava/lang/String;)Z
   */
  public final static String IQGET_SIG2 = IQGET+"(Ljava/lang/String;)Z";

  /**
   * Signature of "initialize" method: initialize(Lcom/stc/jcsre/JCollabController;Ljava/lang/String;I)V
   */
  public final static String INITIALIZE_SIG = INITIALIZE+"(Lcom/stc/jcsre/JCollabController;Ljava/lang/String;I)V";

  /**
   * Signature of "terminate" method: terminate()V
   */
  public final static String TERMINATE_SIG = TERMINATE+"()V";

  /**
   * Signature of "reset" method: reset()Z
   */
  public final static String RESET_SIG = RESET+"()Z";

  /**
   * Signature of "marshal" method: marshal()[B
   */
  public final static String MARSHAL_SIG = MARSHAL+"()[B";

  /**
   * Signature of "unmarshal" method: unmarshal([B)V
   */
  public final static String UNMARSHAL_SIG = UNMARSHAL+"([B)V";

  /**
   * Signature of "rawInput" method: rawInput()[B
   */
  public final static String RAWINPUT_SIG = RAWINPUT+"()[B";

  /**
   * Signature of "topic" method: topic()Ljava/lang/String;
   */
  public final static String TOPIC_SIG = TOPIC+"()Ljava/lang/String;";

  /**
   * Signature of "publications" method: publications()Ljava/util/Vector;
   */
  public final static String PUBLICATIONS_SIG = PUBLICATIONS+"()Ljava/util/Vector;";

  /**
   * Signature of "subscriptions" method: subscriptions()Ljava/util/Vector;
   */
  public final static String SUBSCRIPTIONS_SIG = SUBSCRIPTIONS+"()Ljava/util/Vector;";

  // Currently, these are only for Custom and XML ETDs.

  /**
   * Name of "readProperty" method: readProperty
   */
  public final static String READPROPERTY = "readProperty";

  /**
   * Signature of "readProperty" method: readProperty(Ljava/lang/String;)Ljava/lang/String;
   */
  public final static String READPROPERTY_SIG = READPROPERTY+"(Ljava/lang/String;)Ljava/lang/String;";

  /**
   * Name of "writeProperty" method: writeProperty
   */
  public final static String WRITEPROPERTY = "writeProperty";

  /**
   * Signature of "writeProperty" method: writeProperty(Ljava/lang/String;Ljava/lang/String;)V
   */
  public final static String WRITEPROPERTY_SIG = WRITEPROPERTY+"(Ljava/lang/String;Ljava/lang/String;)V";
}
