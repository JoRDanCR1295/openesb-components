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
 * Copyright 2007-2008 ZAZ Consulting, Inc. All Rights Reserved.
 *
 * END_HEADER - DO NOT EDIT
 */
package com.zaz.ssapi.protocol.tn3270;

import java.util.List;

/**
 *
 * @author Gary Zheng
 */
public interface EhllAPI {
    /*
     * establish connection to application host
     */

    public void ConnectPresentationSpace(String host, String port) throws java.io.IOException;

    /*
     * disconnect from application host
     */
    public void DisconnectPresentationSpace();
//    public void SendKey();
//    public void Wait();

    public void CopyPresentationSpace();

    public void SendPresentationSpace();

    public void BackupPresentationSpace();

    public void ScrollDownPresentationSpace();

    public void ScrollUpPresentationSpace();

    public String GetPresentationSpace(int index);

    public String GetSingleValueByKeyword(String ScreenBuffer, String keyword, String separator, int length);

    public List<String> GetMultiValueByKeyword(String ScreenBuffer, String keyword, String separator, String sub_separator, int length);

    public String GetChildNodeValue(String ScreenBuffer, String treeNode);

    public String GetRelationTableValue(String ScreenBuffer, String tableHead, int offset, int columnBegin, int columnEnd);

    public String GetMapTableValue(String ScreenBuffer, String tableHead, int offset, String key, String separator, int length);
    
    public List<String> GetListTableValue(String ScreenBuffer, int offset, int rowsOfData, int rowBegin, int rowEnd, String columns);

//    public void SearchPresentationSpace();
//    public void QueryCursorLocation();
//    public void CopyPresentationSpaceToString();
//    public void SetSessionParameters();
//    public void QuerySessions();
//    public void Reserve();
//    public void Release();
//    public void CopyOIA();
//    public void QueryFieldAttribute();
//    public void CopyStringToPresentationSpace();

    /*
     * pause for specific time
     */
    public void Pause(Connection conn, int ms);
//    public void QuerySystem();
//    public void ResetSystem();
//    public void QuerySessionStatus();
//    public void StartHostNotification();
//    public void QueryHostUpdate();
//    public void StopHostNotification();
//    public void SearchField();

    /*
     * returns starting position of a target field
     */
    public int FindFieldPosition(int sequence);

    /*
     * returns target field length
     */
    public int FindFieldLength(int sequence);

    /*
     * Write string to strucutred field
     */
    public void CopyStringToField(int sequence, String data);

    /*
     * Read from structured field
     */
    public String CopyFieldToString(int sequence);

    /*
     * position the cursor
     */
    public void SetCursor(int pos);

    public void SetAction(int action);

//    public void StartCloseIntercept();
//    public void QueryCloseIntercept();
//    public void StopCloseIntercept();
//    public void QueryAdditionalFieldAttributeDRB();
//    public void StartKeystokeIntercept();
//    public void GetKey();
//    public void PostInterceptStatus();
//    public void StopKeystrokeIntercept();
//    public void LockPresentationSpaceAPI();
//    public void LockWindowServicesAPI();
//    public void StartCommunicationNotification();
//    public void QueryCommunicationEvent();
//    public void StopCommunicationNotification();
//    public void SendFile();
//    public void ReceiveFile();
//    public void CancelFileTransfer();

    /*
     * convert presentation space position to row/col 
     */
    public int ConvertPosition(ScreenDimension dim);

    /*
     * convert row/col to presentation space position
     */
    public ScreenDimension ConvertRowCol(int position);

//    public void ConnectWindowService();
//    public void QueryWindowCordinates();
//    public void WindowStatus();
//    public void ChangeSwitchListLTName();
//    public void ChangePSWindowName();
//    public void StartPlayingMacro();

    /*
     * establish connection with structured fields
     */
    public FieldConnection ConnectForStructuredFields(Connection conn, int sequence);

    /*
     * disconnect structured field connection
     */
    public void DisconnectForStructuredFields(Connection conn, FieldConnection fconn);

//    public void QueryCommunicationBufferSize();
//    public void AllocateCommunicationBuffer();
//    public void FreeCommunicationBuffer();
//    public void GetRequestCompletion();

    /*
     * read from structured fields
     */
    public Object ReadStructuredFields(FieldConnection fconn);

    /*
     * write to structured fields
     */
    public void WriteStructuredFields(FieldConnection fconn, Object object);
}
