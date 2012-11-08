/*
 * The contents of this file are subject to the terms
 * of the Common Development and Distribution License
 * (the "License").  You may not use this file except
 * in compliance with the License.
 *
 * You can obtain a copy of the license at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.
 * See the License for the specific language governing
 * permissions and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL
 * HEADER in each file and include the License file at
 * https://glassfish.dev.java.net/public/CDDLv1.0.html.  
 *
 * If applicable add the following below this CDDL HEADER,
 * with the fields enclosed by brackets "[]" replaced with 
 * your own identifying information: 
 * "Portions Copyrighted [year] [name of copyright owner]"
 */

/*
 * @(#)mqcrt.h	1.7 08/01/06
 *
 * Copyright 2006 Sun Microsystems, Inc.  All Rights Reserved.
 */ 

#ifndef MQ_CRT_H
#define MQ_CRT_H

/*
 * include all of the MQ C-API public headers 
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

#include "mqtypes.h"
#include "mqversion.h"
#include "mqerrors.h"
#include "mqstatus.h"
#include "mqproperties.h"
#include "mqconnection.h"
#include "mqsession.h"
#include "mqdestination.h"
#include "mqconsumer.h"
#include "mqproducer.h"
#include "mqmessage.h"
#include "mqtext-message.h"
#include "mqbytes-message.h"
#include "mqssl.h"

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* MQ_CRT_H */
