{$REGION 'license'}
{
 * Lock less queues.
 * Copyright (C) 2004-2009 by Boris Popov <borisxm@gmail.com>.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
}
{$ENDREGION}
unit bqueue;

interface

uses
  Classes
  {$IFNDEF FPC}
    ,Windows
  {$ENDIF}
  ;

type
  TQueueNode = TObject;

  {: @abstract(Simple, lock-free FIFO queue implementation.) }
  TBQueueObj = class
  private
    FSize 	: longint;
    FMask 	: longword;
    FHead	: longint;
    FTail	: longint;
    FIndex	: longint;
    FCount	: longint;
    FQueue	: array of TQueueNode;
  public
    {: Allocate queue, the size of queue are always a power of two: QSize := power(2, ASize Power).
      Default size are 1024 elements. }
    constructor Create(ASizePower: integer = 10);
    {: Add object to queue. This call will block if queue size exceeded. }
    procedure Push(const ANode: TQueueNode);
    {: Extract an object from queue. The NIL will be returned if queue is empty. }
    function Pop: TQueueNode;
  end;

implementation

constructor TBQueueObj.create(ASizePower : integer);
begin
  FSize := 1 shl ASizePower;
  FMask := FSize - 1;
  SetLength(FQueue, FSize);
  FHead := 0;
  FTail := 0;
  FIndex := 0;
end;

procedure TBQueueObj.Push(const ANode: TQueueNode);
var
  index, prevtail, next	: longint;
begin
  while true do begin
     index := FCount;
     if index < FSize then begin
       next := InterlockedCompareExchange(FCount, index + 1, index);
       if next = index then
         break;
     end;
  end;

  next := InterlockedIncrement(FIndex);		// reserve space for new element
  index := next - 1;				// index of reserved element
  FQueue[index and FMask] := ANode;		// store new element
  repeat
// The CAS operation in pseudocode:
//    prevtail := FTail;	// save previous tail pointer
//    if index = FTail then	// if no body interrupted us
//      FTail := next;		// set the new tail pointer
    prevtail := InterlockedCompareExchange(FTail, next, index);
  until prevtail = index;	// we're done when new tail succesfully set

end;

function TBQueueObj.Pop: TQueueNode;
var
  oldhead, newhead	: longint;
begin
  result := nil;
  while FHead <> FTail do begin			// try to extract an element if queue is not empty
    oldhead := FHead;				// record current head pointer
    // now try to perform exactly one step of head forward
    newhead := InterlockedCompareExchange(FHead, oldhead + 1, oldhead);
    if newhead = oldhead then begin		// check if no one stepped in
      InterLockedDecrement(FCount);
      result := FQueue[oldhead and FMask];
      exit;
    end;
    // otherwise try again and again
  end;
end;

end.
