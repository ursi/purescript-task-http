exports.requestImpl = request => aC => xC => () => {
	fetch(request.url, {
		body: request.body.size === 0 ? undefined : request.body,
		headers: request.headers,
		method: request.method,
	}).then(res => {
		const headers = {};

		for (const h of res.headers)
			headers[h[0]] = h[1];

		res.blob().then(blob => aC({
			body: blob,

			status: {
				code: res.status,
				message: res.statusText,
			},

			headers,
		}));
	});

	return () => {};
};
