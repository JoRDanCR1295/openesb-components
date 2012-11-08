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
 * @(#)AspectMap.java 
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.engine.aspect.endpoint.factory;

/**
 * this holds the aspectmap.xml constants
 * 
 * @author Sujit Biswas
 * 
 */
public class AspectMap {

	public static final String EXCHANGE_TYPE = "exchangeType";

	public static final String REQUEST_REPLY_ELEMENT = "requestReplyService";

	public static final String FILTER_ONE_WAY_ELEMENT = "filterOneWay";

	public static final String FILTER_REQUEST_REPLY_ELEMENT = "filterRequestReply";

	public static final String INPUT_ELEMENT = "input";

	public static final String OUTPUT_ELEMENT = "output";

	public static final String FILE_ATTR = "file";

	public static final String PARTNERLINK_ATTR = "partnerLink";

	public static final String SERVICE_NAME_ATTR = "serviceName";

	public static final String PORT_NAME_ATTR = "portName";

	public static final String ROLE_NAME_ATTR = "roleName";

	public static final String PORTTYPE_ATTR = "portType";

	public static final String OPERATION_ATTR = "operation";

	public static final String MESSAGE_TYPE_ATTR = "messageType";
        
        public static final String OUTPUT_REQUEST = "request";
        
        public static final String OUTPUT_RESPONSE = "response";

	public static final String ID_ATTR = "ID";

	public static final String CACHE_JBI_ATTR = "transformJBI";

	public static final String ADVICE = "advice";

	public static final String ADVICE_ATTR_TYPE = "type";

	public static final String ASPECT = "aspect";

	public static final String ORDER = "order";

	public static final String ADVICE_LOGGING = "logging";

	public static final String ADVICE_CACHE = "cache";

	public static final String ADVICE_AUTO_RECONNECT = "autoReconnect";

	public static final String ADVICE_QUEUEING = "queueing";

	public static final String ADVICE_MESSAGE_TRACKING = "messageTracking";

	public static final String ADVICE_TEE = "tee";

	public static final String ADVICE_CONTENT_BASED_ROUTING = "contentBasedRouting";

	public static final String ADVICE_THROTTLING = "throttling";

	public static final String DESCRIPTOR_FILE = "aspectmap.xml";
}
