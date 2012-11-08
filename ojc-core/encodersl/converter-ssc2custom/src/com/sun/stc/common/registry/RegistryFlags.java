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
 * @(#)RegistryFlags.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.common.registry;

/**
 * This type was created in VisualAge.
 */
public interface RegistryFlags 
{

//////////// NEW RETRIEVE TAGS ////////////////
public static final int RETRIEVE_IGNORELOCALFILE     					=	0x00000001;
public static final int RETRIEVE_JUSTFILLFILEPATH    					=	0x00000002;
public static final int RETRIEVE_JUSTOPENSCHEMA      					=	0x00000004;
public static final int RETRIEVE_JUSTSANDBOX      					    =	0x00000008;
public static final int RETRIEVE_EXISTSONLY          					=	0x00000010;
public static final int RETRIEVE_MASK                					=	0x0000ff00;
public static final int RETRIEVE_RETVFORUPDATE       					=	0x00000100;
public static final int RETRIEVE_IGNORELOCK          					=	0x00000200;
public static final int RETRIEVE_BYPASSSANDBOX       					=	0x00000400;


//////////// NEW COMMIT TAGS ////////////////
public static final int COMMIT_JUSTSANDBOX      				    =	0x00000008;
public static final int COMMITFILE_MASK                 			=	0x0ff00000;
public static final int COMMITFILE_UNEDIT               			=	0x00100000;
public static final int COMMITFILE_DELETEFROMSANDBOX    			=	0x00200000;
public static final int COMMITFILE_DELETEFROMRUNTIME    			=	0x00400000;
public static final int COMMITFILE_PROMOTETORUNTIME     			=	0x00800000;
public static final int COMMITFILE_BYPASSSANDBOX        			=	RETRIEVE_BYPASSSANDBOX;


//////////// NEW ENUM TAGS ////////////////
public static final int ENUMFILE_JUSTOPENSCHEMA  					= RETRIEVE_JUSTOPENSCHEMA;
public static final int ENUMFILE_JUSTSANDBOX      					= RETRIEVE_JUSTSANDBOX;


//////////// NEW REPOSITORY RESPONSE TAGS ////////////////
public static final int COMMITFILE_SUCCESS              			=	0x00000001;
public static final int COMMITFILE_FAILURE              			=	0x00000002;
public static final int COMMITFILE_LOCKNOTIFY           			=	0x00000004;
public static final int RETRIEVEFILE_SUCCESS              		    =	0x00000001;
public static final int RETRIEVEFILE_FAILURE              		    =	0x00000002;
public static final int RETRIEVEFILE_LOCKNOTIFY           		    =	0x00000004;
}
