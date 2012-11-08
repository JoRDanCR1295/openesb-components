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
 * @(#)RegistryConstants.java 
 *
 * Copyright 2004-2008 Sun Microsystems, Inc. All Rights Reserved.
 * 
 * END_HEADER - DO NOT EDIT
 */

package com.sun.stc.common.registry;

public interface RegistryConstants extends RegistryFlags
{
  public static final String separator            = System.getProperty("file.separator");
  public static final String home_dir             = System.getProperty("user.home");
	
  public static final int LOGICAL_NAME      = 64;
  public static final int SHARE             = 64;
  public static final int PATH              = 256;
  public static final int FILE              = 64;
  public static final int HOST_NAME         = 64;
  public static final int DOMAIN_NAME       = 64;
  public static final int FUNC_NAME         = 64;
  public static final int USER_NAME         = 64;
  public static final int PASSWORD          = 256;
  public static final int MAX_SCHEDULE      = 256;
  public static final int AUTH_CHALL_SIZE   = 8;
  public static final int PRIVILEGES        = 18; //added one for debug privilege

  												  //added one for Modify Module List			

  public static final int DB_NAME           = 64;
  public static final int DB_SCHEMA         = 64;
  public static final int DB_TABLE_NAME     = 64;
  public static final int DB_LOGIN_NAME     = 64;
  public static final int DB_LOGIN_PASS     = 256;

  public static final int REG_ACK           = 8;
  public static final int REG_SCHEMA        = 64;
  public static final int SYS_CONTROL_INFO  = 64;
  public static final int STAT_VALUE_NAME   = 32;
  public static final int HASH              = 32;
  public static final int MAX_ENCRYPTION_KEY= 1024;

  public static final int BOOLEAN           = 1;  
  public static final int BYTE              = 1;
  public static final int WORD              = 2;
  public static final int INT               = 4;
  public static final int LONG              = 4;
  public static final int QUAD_STRING       = 4;
  public static final int UID               = 16;
  public static final int FILE_REF          = 328;
  public static final int CALLER_ID         = 472;  
  public static final int AUTHENTICATION    = 24;  
  public static final int DBREF             = 464; 
  public static final int REGISTRY_TIME     = 8;

  public static final int ACL               = 74; //added one for debug privilege
  												  //added one for MML privilege
  public static final int COLLABMAP			= 592;
  public static final int CONTROL_BROKER    = 480;
  public static final int CONNECTION_POINT	= 624;
  public static final int ENCRYPTION_KEY    = 1416;
  public static final int GROUP             = 120;
  public static final int GROUPENT          = 56;
  public static final int HOST              = 240;
  public static final int IQSERVICE         = 1400;
  public static final int IQUEUE            = 1456;
  public static final int LAYENT            = 96;
  public static final int LAYOUT            = 104;
  public static final int LOG               = 128;
  public static final int MESSAGE           = 1016;
  public static final int MODULE            = 1480; 
  public static final int MONITOR           = 224;
  public static final int MSGTRMSG          = 96;
  public static final int PUBLISHER         = 496;
  public static final int PUBMSG            = 136;
  public static final int PUBSUBLAYENT      = 128;
  public static final int REGISTRY_ACQUIRE  = 560; 
  public static final int REGISTRY_TABLE_OBJ= 192;  
  public static final int RESOURCE          = 120;
  public static final int ROLE              = 104;
  public static final int SCHEDULE          = 632;
  public static final int SUBMSG            = 80;
  public static final int SUBSCRIBER        = 504;
  public static final int TRANSSERVICE      = 1264;
  public static final int TRANSLATION       = 1352;
  public static final int USER              = 368;
  public static final int USER_ROLE         = 48;
  
  public static final int REP_RETRIEVE_REQ  = 388;
  public static final int REP_RETRIEVE_RESP = 64;
  public static final int REP_COMMIT_REQ    = 396;
  public static final int REP_COMMIT_RESP   = 472;
  public static final int REP_ENUM_REQ      = 552;  // 336
  public static final int REP_ENUM_RESP     = 96;
  public static final int REP_STREAM        = 16;
  
  //////////// NEW RECORDS ///////////////////////////////
  public static final int FILE_ENUM_REQ         = 336;
  public static final int FILE_ENUM_RESP        = 96;
  public static final int FILE_LOCK_NOTIFY      = 72;
  public static final int FILE_REQUEST          = 392;
  public static final int FILE_DATA_TEMPLATE    = 388;
  
  public static final int REGMSG_HANDLE         = 8;  //16;
  public static final int REGISTRY_AUTH         = 24; //16
  public static final int REGMSG_ROW_TEMPLATE   = 12;
  
  //public static final String TAG            = "REG4";
  //public static final String ACK            = "ACK ";
  public static final String ATHC           = "ATHC";
  public static final String ATHR           = "ATHR";
  public static final String CMDR           = "CMDR";
  public static final String CMDA           = "CMDA";
  public static final String CMDC           = "CMDC";
  public static final String CTRL_STOP      = "CSTP";
  //public static final String ENUM_SCHEMAS   = "CES ";
  //public static final String ACQUIRE        = "Acqr";
  //public static final String RELEASE        = "Rlse";
  //public static final String PORT_QUERY     = "PQRY";
  //public static final String PORT_RESP      = "PQRS";
  //public static final String ADD            = "Add";
  //public static final String FIND           = "Find";
  //public static final String ENUM           = "Enum";
  //public static final String UPDATE         = "Upd";
  //public static final String DELETE         = "Del";
  //public static final String FROM_HANDLE    = "FrHd";
  public static final String CLOSE_HANDLE   = "ClHd";
  public static final String ADD_INTEREST   = "AInt";


  
  //////////// NEW COMMANDS ////////////////
  public static final String TAG            = "REG5";
  public static final String ACK            = "gACK";
  public static final String ACQUIRE        = "rAQR";
  public static final String ADD            = "rADD";
  public static final String AUTH_CHALLENGE = "gATH";
  public static final String AUTH_RESPONSE  = "gARP";
  public static final String DELETE         = "rDEL";
  public static final String ENUM           = "rENM";
  public static final String ENUM_SCHEMAS   = "cES ";
  public static final String FIND           = "rFND";
  public static final String FROM_HANDLE    = "rFHD";
  public static final String PORT_QUERY     = "gPQY";
  public static final String PORT_RESP      = "gPQY";
  public static final String RELEASE        = "rRLS";
  public static final String UPDATE         = "rUPD";
  
  public static final String COMMIT_FILE        = "pRC ";
  public static final String ENUM_FILES     		= "pENM";
  public static final String FILE_SOFT_LOCK     = "pNSL";
  public static final String RETRIEVE_FILE      = "pRTR";
  

  
  
  
  public static final String REPR           = "REPR";
  public static final String REPA           = "REPA";
  public static final String RENM           = "RENM";
  public static final String RRTV           = "RRtv";
  public static final String RCMT           = "RCmt";

  public static final int REGISTRY_HEADER                 = 16;
  public static final int COMPLETE_HEADER_LENGTH          = 37;
  public static final int COMPLETE_RETURN_HEADER_LENGTH   = 29;
  public static final int REP_HEADER_LENGTH               = 25;
  public static final int PACKET                          = 4096;
  
  public static final int HASH_NONE                       = 0;
  public static final int HASH_MD5                        = 1;
  public static final int HASH_SHA1                       = 2;
  public static final int ANY_SIZE_ARRAY                  = 1;
  public static final int REGRETFILE_IGNORELOCALFILE      = 1;
  public static final int CB_CONNECTION_HEADER_LENGTH     = 67;
  
  public static final byte FALSE            = 0;
  public static final byte TRUE             = 1;
  
  public static final int MAX_STREAM_DATA   = (((PACKET - REP_STREAM) - REP_HEADER_LENGTH) - 100);
  public static final int NO_HANDLE         = 0;
  public static final int NO_OBJECT         = 0;
  public static final int SUCCESS           = 0;
  public static final int VALID             = 0;
  public static final int INVALID           = -1;

  public static final int DEFAULT_PORT			= 23001;

}