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
 * @(#)JCSConstants.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.jcs;

/**
 * This class contains various constants for the JCS package.
 */
public class JCSConstants {

    /**
     * Hidden constructor.
     * We don't want to be able to instantiate this class.
     */
    private JCSConstants() {}

    public static final String resetMethodComment =
	"Clears the contents of this ETD instance.  " +
	"This method sets the values of all fields to null.";

    public static final String availableMethodComment =
	"Checks to see whether this ETD instance contains any data.  " +
	"Use this method when there are multiple triggering subscriptions " +
	"and the Collaboration needs to know which Event Type triggered it.";

    public static final String nextMethodComment =
	"Checks to see whether there is more data waiting to be processed.  " +
	"If so, the data is unmarshalled into this ETD instance.  " +
	"Use next() in conjunction with Event Linking and Sequencing (ELS).  " +
	"After ELS has handed off a vector of Events to the " +
	"executeBusinessRules() placeholder, you can invoke next() to " +
	"advance over the vector.";

    public static final String receiveMethodComment =
	"Applies to inbound Event instances only.  Pulls any data waiting " +
	"in the IQ for the current Event instance and unmarshals (parses) " +
	"it into this Event instance.  If a topicName is specified, " +
	"receive pulls only data for a particular Event Type.";

    public static final String topicNameParamComment =
	"The name of the topic (Event Type) residing in the IQ.";

    public static final String sendMethodComment =
	"Applies to outbound Event instances only.  Sends the entire data " +
	"content of this ETD instance as output in its marshalled " +
	"(serialized) form.  If topicName is specified, send only sends " +
	"data for the specified Event Type; otherwise, it sends to all " +
	"topics associated with the instance.";

    public static final String rawInputMethodComment =
	"This represents the original data contents of this ETD instance " +
	"before it was unmarshalled.";

    public static final String topicMethodComment =
	"Retrieves the name of the Event Type for this ETD instance.";

    public static final String publicationsMethodComment =
	"Retrieves the names of all Event Types published by this ETD " +
	"instance.";

    public static final String subscriptionsMethodComment =
	"Retrieves the names of all Event Types this ETD instance " +
	"subscribes to.";

    public static final String marshalMethodComment =
	"Organizes the data of this ETD instance into a serialized byte " +
	"stream (a BLOB).  Since this flattens the instance -- that is, " +
	"converts it from a hierarchical structure to a linear one -- " +
	"marshal() allows a node with subnodes to be copied to a single " +
	"destination node.";

    public static final String blobParamComment  =
	"byte array of the BLOB to be unmarshalled";

    public static final String unmarshalMethodComment =
	"De-serializes a particular byte stream and parses it into an " +
	"appropriate hierarchical form for this ETD instance.";

    public static final String readPropNameParamComment =
	"Name of the property to read.  A property of this name must have " +
	"been previously written to message header by a call to " +
	"writeProperty().";

    public static final String readPropertyMethodComment =
	"Retrieves the value for a particular property that was assigned " +
	"to this ETD instance.  Used in conjunction with writeProperty().  " +
	"Together, these two methods allow you to store data about an " +
	"Event instance, outside of the Event itself.  For example, " +
	"writeProperty() and readProperty() can be used to store the name " +
	"of the file containing the Event instance, the date and time it " +
	"was published, the name of the publishing application, or even a " +
	"checksum or signature.  They can also be used to calculate and " +
	"store key fields used by Event Linking and Sequencing (ELS).  " +
	"readProperty() is only applicable when using JMS eWay connections.";

    public static final String writePropNameParamComment =
	"User-defined name of a property to store in the message header.  " +
	"Applies only to Events being sent to a JMS e*Way Connection.";

    public static final String writePropValueParamComment =
	"The value to store for this property.  Applies only to Events being " +
	"sent to a JMS e*Way Connection.";

    public static final String writePropertyMethodComment =
	"Creates a user-defined property for Events in this ETD instance.  " +
	"Used in conjunction with readProperty().  Together, these two " +
	"methods allow you to store data about an Event instance, outside " +
	"of the Event itself.  For example, writeProperty() and " +
	"readProperty() can be used to store the name of the file " +
	"containing the Event instance, the date and time it was published, " +
	"the name of the publishing application, or even a checksum or " +
	"signature.  They can also be used to calculate and store key " +
	"fields used by Event Linking and Sequencing (ELS).  " +
	"writeProperty() is only applicable when using JMS eWay connections.";

    public static final String marshalExceptionClass =
	"com.sun.stc.jcsre.MarshalException";
    public static final String marshalExceptionComment =
	"Unable to marshal ETD.";

    public static final String unmarshalExceptionClass =
	"com.sun.stc.jcsre.UnmarshalException";
    public static final String unmarshalExceptionComment =
	"Unable to unmarshal BLOB into ETD.";

    public static final String collabDataExceptionClass =
	"com.sun.stc.common.collabService.CollabDataException";
    public static final String collabDataExceptionComment =
	"thrown when JMsgObject data cannot be unmarshalled.";
    public static final String collabDataExceptionComment2 =
	"thrown when the ETD data cannot be marshalled.";
    public static final String collabDataExceptionComment3 =
	"unknown.";

    public static final String getMethodComment =
	"get the value";
    public static final String setMethodComment =
	"set the value";
    public static final String hasMethodComment =
	"returns true if the {0} is set";
    public static final String omitMethodComment =
	"the has{0} method will return false after this call";

    public static final String indexParamComment =
	"the index";
    public static final String valueParamComment =
	"the value";
    public static final String valueArrayParamComment =
	"array containing elements";
    public static final String getIndexMethodComment =
	"get the element at the specfied index";
    public static final String setIndexMethodComment =
	"set the element at the specfied index";
    public static final String getArrayMethodComment =
	"get an array containing the elements";
    public static final String setArrayMethodComment =
	"set elements to those contained in the specified array";
    public static final String countMethodComment =
	"returns the number of elements";
    public static final String removeIndexMethodComment =
	"remove the element at the specfied index";
    public static final String addMethodComment =
	"add the element";
    public static final String addIndexMethodComment =
	"add the element at the specfied index";
    public static final String clearMethodComment =
	"clears all elements out of list";
}
