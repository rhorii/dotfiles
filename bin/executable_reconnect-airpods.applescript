#!/usr/bin/osascript

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Reconnect AirPods
# @raycast.mode silent

# Optional parameters:
# @raycast.icon 🎧

# Documentation:
# @raycast.author rhorii
# @raycast.authorURL https://github.com/rhorii

use framework "IOBluetooth"
use scripting additions

set AirPodsName to "AirPods"

on getFirstMatchingDevice(deviceName)
	repeat with device in (current application's IOBluetoothDevice's pairedDevices() as list)
		if (device's nameOrAddress as string) contains deviceName then return device
	end repeat
end getFirstMatchingDevice

on reconnectDevice(device)
	if not (device's isConnected as boolean) then
		device's openConnection()
		return "Connecting " & (device's nameOrAddress as string)
	else
		return "Already connected " & (device's nameOrAddress as string)
	end if
end reconnectDevice

return reconnectDevice(getFirstMatchingDevice(AirPodsName))
