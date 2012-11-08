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
 * @(#)FileBasedJMSMessageReceiver.java
 *
 * Copyright 2004-2007 Sun Microsystems, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */

package com.sun.jbi.performance.jmsbd;

/**
 *
 * @author Bing Lu
 */
public interface Configuration {
    String NAME = "name";
    String JMS_SERVER_HOST_NAME = "jms-server-host-name";
    String JMS_SERVER_PORT = "jms-server-port";
    String JMS_QUEUE = "jms-queue";
    String BATCH_SIZE = "batch-size";
    String TEMPLATE_FILE = "template-file";
    String MESSAGE_DELIVERY_MODE = "message-delivery-mode";
    String DONE_TOKEN = "done-token";
}
