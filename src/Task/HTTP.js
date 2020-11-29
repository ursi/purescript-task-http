exports.requestImpl = request => aC => xC => () => {
	fetch(request.url, {
		headers: request.headers
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
